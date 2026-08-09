//! The approval-ledger vocabulary: pure data plus serde, no behavior.
//!
//! These are the types the approval ledger (`docs/approval-ledger.md` — the
//! "spec" cited throughout this module) is spoken in: requests, grants, the
//! append-only [`LedgerEntry`] log, and the small id/enum types they name.
//! There is deliberately no ledger here — no state machine, no I/O, no
//! matching. The ledger core (`Ledger`, the `Requester`/`Approvals`/
//! `ApproverHandle` split, the transition tables) belongs to `kaish-kernel`,
//! and nothing here depends on it or on `kaish-glob` — a [`ResourcePattern`]
//! carries pattern *data* only.
//!
//! Two structural guarantees hold everywhere in this module:
//!
//! - **[`Token`] is never a field of any other type here, and never
//!   implements `Serialize`/`Deserialize`.** The redemption credential lives
//!   only in the kernel's credential index, keyed by [`RequestId`] (spec
//!   §A.2). Adding a `token: Token` field to any serialized type fails to
//!   compile — `Token` has no serde impls to derive against. The exhaustive
//!   field destructures in this module's tests pin each wide record's field
//!   list, so a new field cannot land unreviewed.
//! - **[`ApprovalRequestDraft`] has no `principal`, `capture`, `id`, `context`,
//!   or `requested_at` field.** A plugin building a request through
//!   [`ApprovalRequest::builder`] cannot forge any of them — the draft type has
//!   nowhere to put them. [`ApprovalRequestDraft::stamp`] is the only path to a
//!   postable [`ApprovalRequest`], and only a caller holding the kernel context
//!   that knows those values can call it (spec §D.1).

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::SystemTime;

use serde::{Deserialize, Serialize};
use thiserror::Error;

// ───────────────────────── Identity: RequestId ─────────────────────────

/// The request's public name. Format `"req_{epoch:8hex}_{seq}"`, e.g.
/// `"req_9c1a4f2e_42"` — underscores throughout and no other separator,
/// because a hyphen ends a terminal's double-click selection and this id
/// exists to be copied. There is no short form: an id is printed in full and
/// accepted in full, so it can never be ambiguous between sessions sharing a
/// ledger (spec §A.2).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct RequestId(String);

/// Why a string failed to parse as a [`RequestId`].
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum RequestIdParseError {
    /// Missing the `"req_"` prefix.
    #[error("request id {0:?} does not start with \"req_\"")]
    BadPrefix(String),
    /// Not exactly `req_<epoch>_<seq>` — includes any short form (e.g. the
    /// epoch alone, with no `_<seq>` suffix).
    #[error("request id {0:?} must have the shape req_<8hex>_<seq> — there is no short form")]
    BadShape(String),
    /// The epoch segment is not exactly 8 lowercase hex characters.
    #[error("request id {0:?} epoch must be exactly 8 lowercase hex characters")]
    BadEpoch(String),
    /// The sequence segment is not a plain decimal integer.
    #[error("request id {0:?} sequence must be a decimal integer")]
    BadSeq(String),
}

impl RequestId {
    /// Build a `RequestId` from a ledger epoch and a monotonic sequence
    /// number. `epoch` renders as 8 lowercase hex digits. Kernel-internal in
    /// practice — the ledger core is the only allocator (spec §A.2).
    pub fn new(epoch: u32, seq: u64) -> Self {
        Self(format!("req_{epoch:08x}_{seq}"))
    }

    /// Parse a `RequestId` from its full-form text. Rejects anything not
    /// full-form — there is no short form to accept (spec §A.2).
    pub fn parse(s: &str) -> Result<Self, RequestIdParseError> {
        let rest = s
            .strip_prefix("req_")
            .ok_or_else(|| RequestIdParseError::BadPrefix(s.to_string()))?;
        let (epoch_hex, seq_str) = rest
            .split_once('_')
            .ok_or_else(|| RequestIdParseError::BadShape(s.to_string()))?;
        let epoch_ok = epoch_hex.len() == 8
            && epoch_hex
                .bytes()
                .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b));
        if !epoch_ok {
            return Err(RequestIdParseError::BadEpoch(s.to_string()));
        }
        let seq_ok = !seq_str.is_empty() && seq_str.bytes().all(|b| b.is_ascii_digit());
        if !seq_ok {
            return Err(RequestIdParseError::BadSeq(s.to_string()));
        }
        // Re-render instead of storing the input: equality and lookup are
        // string-based, so "req_9c1a4f2e_042" must become the canonical
        // "req_9c1a4f2e_42" or a hand-typed leading zero fails `approvals
        // grant` with "not found" against a kernel-allocated id.
        let seq: u64 = seq_str
            .parse()
            .map_err(|_| RequestIdParseError::BadSeq(s.to_string()))?;
        Ok(Self(format!("req_{epoch_hex}_{seq}")))
    }

    /// The id's text form, e.g. `"req_9c1a4f2e_42"`.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The allocation sequence this id carries — the `42` in
    /// `req_9c1a4f2e_42`.
    ///
    /// This is what orders requests chronologically. Sorting the id *text*
    /// does not: `req_9c1a4f2e_10` sorts before `req_9c1a4f2e_9`, so a tenth
    /// request would list ahead of the ninth on every surface that
    /// enumerates them.
    ///
    /// Returns 0 for a value holding a non-canonical id. Both constructors
    /// render the canonical form, so that cannot happen through the public
    /// API; 0 sorts such a value first rather than panicking a listing.
    pub fn seq(&self) -> u64 {
        self.0
            .rsplit_once('_')
            .and_then(|(_, seq)| seq.parse().ok())
            .unwrap_or(0)
    }
}

impl std::fmt::Display for RequestId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::str::FromStr for RequestId {
    type Err = RequestIdParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl TryFrom<String> for RequestId {
    type Error = RequestIdParseError;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::parse(&s)
    }
}

impl From<RequestId> for String {
    fn from(id: RequestId) -> String {
        id.0
    }
}

// ───────────────────────── Identity: Token ─────────────────────────

/// The redemption credential. 128 bits from `getrandom`, 32 lowercase hex
/// (minted in `kaish-kernel`, kaish #259) — this type only carries the value
/// once minted, so it stays dependency-light.
///
/// Deliberately has **no** `Display`, `Serialize`, or `Deserialize` impl, and
/// its `Debug` impl prints only a redacted prefix. The credential lives ONLY
/// in the kernel's credential index, keyed by [`RequestId`]; it is never a
/// field of any [`LedgerEntry`] or any other public type in this module
/// (spec §A.2), and never serialized to a sink or the VFS. A `Debug` impl
/// that prints the raw value would be a bug precisely because `{:?}` is the
/// format callers reach for without thinking — that is the one place a
/// secret leaks by accident, so it is the one place this type refuses.
#[derive(Clone, PartialEq, Eq)]
pub struct Token(String);

impl Token {
    /// Wrap a raw credential value already minted elsewhere.
    pub fn new(raw: impl Into<String>) -> Self {
        Self(raw.into())
    }

    /// First 4 characters — enough to correlate a `TokenRejected` entry with
    /// the grant it was aimed at ([`Grant::token_prefix`]), never enough to
    /// redeem (spec §A.4).
    pub fn token_prefix(&self) -> String {
        self.0.chars().take(4).collect()
    }

    /// The raw credential. Named loudly, not `as_str`/`AsRef`, so a call site
    /// that reveals the secret is a `grep`-able `.reveal(` rather than an
    /// invisible trait-method call.
    pub fn reveal(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token(redacted, prefix={})", self.token_prefix())
    }
}

// ───────────────────────── Identity: the rest ─────────────────────────

/// One execution reserved against a grant. Unique within a ledger. Allocated
/// by the reservation that creates the attempt, never by a caller (spec
/// §A.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct AttemptId(u64);

impl AttemptId {
    /// Wrap a raw attempt id. Kernel-internal in practice — see the type doc.
    pub fn new(raw: u64) -> Self {
        Self(raw)
    }

    /// The underlying sequence number.
    pub fn get(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Display for AttemptId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The id of a [`StandingGrant`]. Kernel-allocated on `StandingIssued`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StandingId(u64);

impl StandingId {
    /// Wrap a raw standing-grant id. Kernel-internal in practice.
    pub fn new(raw: u64) -> Self {
        Self(raw)
    }

    /// The underlying sequence number.
    pub fn get(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Display for StandingId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The id of a [`Subscription`] (spec §C.5). Kernel-allocated on
/// registration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SubscriptionId(u64);

impl SubscriptionId {
    /// Wrap a raw subscription id. Kernel-internal in practice.
    pub fn new(raw: u64) -> Self {
        Self(raw)
    }

    /// The underlying sequence number.
    pub fn get(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Display for SubscriptionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A dotted operation id (`"fs.remove"`, `"trash.empty"`, `"git.push"`).
/// In-tree operations come from a closed enum in `kaish-kernel`; plugins
/// register a namespace prefix at tool-registration time and build ids
/// through [`Self::namespaced`] (spec §A.6, §D.1).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct OperationId(String);

/// Namespace prefixes reserved for in-tree kernel operations. A plugin
/// cannot register under these — see [`OperationId::namespaced`].
const RESERVED_OPERATION_PREFIXES: &[&str] = &["fs", "trash", "cmd"];

/// Why an [`OperationId`] could not be built.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum OperationIdError {
    /// The dotted id (or one of its parts) was empty.
    #[error("operation id must not be empty")]
    Empty,
    /// A plugin tried to register under a kernel-reserved namespace.
    #[error("operation namespace {0:?} is reserved for in-tree kernel operations")]
    ReservedPrefix(String),
}

impl OperationId {
    /// Build an `OperationId` from an already-dotted string (`"git.push"`).
    /// Rejects only an empty string — reserved-prefix enforcement is
    /// [`Self::namespaced`]'s job, run once at plugin registration, not
    /// re-checked on every request.
    pub fn new(dotted: impl Into<String>) -> Result<Self, OperationIdError> {
        let dotted = dotted.into();
        if dotted.is_empty() {
            return Err(OperationIdError::Empty);
        }
        Ok(Self(dotted))
    }

    /// Build a plugin-namespaced `OperationId`: `namespaced("git", "push")`
    /// → `"git.push"`. Rejects the reserved `fs`/`trash`/`cmd` prefixes,
    /// which belong to the kernel — a plugin cannot pose as an in-tree
    /// operation (spec §A.6).
    pub fn namespaced(prefix: &str, rest: &str) -> Result<Self, OperationIdError> {
        if prefix.is_empty() || rest.is_empty() {
            return Err(OperationIdError::Empty);
        }
        if RESERVED_OPERATION_PREFIXES.contains(&prefix) {
            return Err(OperationIdError::ReservedPrefix(prefix.to_string()));
        }
        Ok(Self(format!("{prefix}.{rest}")))
    }

    /// The id's dotted text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for OperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// A glob-shaped operation match, e.g. `"git.commit"` or `"fs.*"`
/// (`StandingGrant::operations`). Pattern *data* only — matching is
/// `kaish-glob`'s job (kaish-types must not depend on it).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct OperationPattern(String);

impl OperationPattern {
    /// Wrap a raw pattern string.
    pub fn new(pattern: impl Into<String>) -> Self {
        Self(pattern.into())
    }

    /// The pattern's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for OperationPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

// ───────────────────────── Principal ─────────────────────────

/// Who is asking, or who decided. Appears on both the request (who asked)
/// and the grant (who decided) — spec §A.3.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Principal {
    /// Opaque identifier within `kind`'s namespace.
    pub id: String,
    /// What kind of actor this is.
    pub kind: PrincipalKind,
}

impl Principal {
    /// Build a principal.
    pub fn new(id: impl Into<String>, kind: PrincipalKind) -> Self {
        Self { id: id.into(), kind }
    }
}

/// What kind of actor a [`Principal`] is. Seeded by
/// `KernelConfig::with_principal`, defaulting to `Unknown` (spec §A.3).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PrincipalKind {
    /// An AI agent.
    Agent,
    /// A human.
    Human,
    /// Unattended automation (a cron-style rule, CI).
    Automation,
    /// Not classified.
    #[default]
    Unknown,
}

// ───────────────────────── Scope ─────────────────────────

/// The kernel a request was raised in. Minted per kernel at construction
/// (spec §A.7) — [`Self::mint`] hands out a fresh value per call, so two
/// kernels in one process never share one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct KernelId(u64);

/// Backing counter for [`KernelId::mint`]. Process-wide and monotonic: a
/// kernel id must be unique within the process, and nothing outside the
/// process ever reads one.
static NEXT_KERNEL_ID: AtomicU64 = AtomicU64::new(1);

impl KernelId {
    /// Mint a fresh kernel id. One call per kernel construction.
    pub fn mint() -> Self {
        Self(NEXT_KERNEL_ID.fetch_add(1, Ordering::Relaxed))
    }

    /// Wrap a raw id — for an embedder that assigns its own kernel numbering.
    pub fn new(raw: u64) -> Self {
        Self(raw)
    }

    /// The underlying number.
    pub fn get(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Display for KernelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The conversation, connection, or task a kernel is serving. Supplied by
/// the embedder; `None` on a single-session kernel like the REPL (spec §A.7).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SessionId(String);

impl SessionId {
    /// Name a session.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// The session's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SessionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// The actor an operation runs on behalf of, when the embedder distinguishes
/// one from the session — a subagent under a user (spec §A.7).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PrincipalId(String);

impl PrincipalId {
    /// Name an actor.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// The actor's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for PrincipalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Which kernel, session, and actor a request belongs to (spec §A.7).
///
/// Mandatory on every request and carried onto every [`LedgerRecord`] about
/// it: a helper hosting several sessions must never need an external map to
/// answer "whose request is this?" — answering it from an external map is how
/// a confused deputy is built.
///
/// Scoping is API hygiene, not a process boundary. A scoped handle stops a
/// session's code from reaching another session's requests by accident; it
/// does not stop hostile Rust in the same process, which can reach anything
/// the process can (spec §A.2's threat model applies here unchanged).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ApprovalScope {
    /// The kernel that raised it.
    pub kernel_id: KernelId,
    /// The conversation, connection, or task the kernel is serving. `None`
    /// for a single-session kernel.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub session_id: Option<SessionId>,
    /// The actor on whose behalf the operation runs, when the embedder
    /// distinguishes one from the session.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub actor_id: Option<PrincipalId>,
}

impl ApprovalScope {
    /// A single-session kernel's scope: a kernel id and nothing else.
    pub fn kernel(kernel_id: KernelId) -> Self {
        Self {
            kernel_id,
            session_id: None,
            actor_id: None,
        }
    }

    /// Name the session this scope serves.
    pub fn with_session(mut self, session: SessionId) -> Self {
        self.session_id = Some(session);
        self
    }

    /// Name the actor this scope runs on behalf of.
    pub fn with_actor(mut self, actor: PrincipalId) -> Self {
        self.actor_id = Some(actor);
        self
    }

    /// Whether a handle restricted to `session` may see or decide this scope.
    ///
    /// Exact match on the session id, and a scope with **no** session is
    /// invisible to every scoped handle: an unattributed request belongs to
    /// the kernel, not to whichever session asks first.
    pub fn in_session(&self, session: &SessionId) -> bool {
        self.session_id.as_ref() == Some(session)
    }
}

impl std::fmt::Display for ApprovalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "kernel {}", self.kernel_id)?;
        if let Some(session) = &self.session_id {
            write!(f, ", session {session}")?;
        }
        if let Some(actor) = &self.actor_id {
            write!(f, ", actor {actor}")?;
        }
        Ok(())
    }
}

// ───────────────────────── Replay binding ─────────────────────────

/// A digest over what was judged (spec §A.9). The kernel computes it; this
/// type only carries the value, so `kaish-types` stays dependency-light.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PlanDigest(String);

impl PlanDigest {
    /// Wrap an already-computed digest.
    pub fn new(hex: impl Into<String>) -> Self {
        Self(hex.into())
    }

    /// The digest's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for PlanDigest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Which sandbox profile was in force, when the embedder names them
/// (spec §A.9).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SandboxProfileId(String);

impl SandboxProfileId {
    /// Name a sandbox profile.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// The profile's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SandboxProfileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// The context a grant was decided against (spec §A.9).
///
/// > A grant may be redeemed only by an attempt whose binding matches the one
/// > the grant was decided against. Anything else is a new request.
///
/// This does not replace the redemption-time precondition check (spec §B.4);
/// the two answer different questions. A [`StateResolver`] asks whether the
/// *world* still matches what was claimed. The binding asks whether the
/// *operation* is still the one that was judged — a resolver cannot cover a
/// cwd change, because nothing declared the cwd as a precondition.
///
/// [`StateResolver`]: https://docs.rs/kaish-tool-api/latest/kaish_tool_api/trait.StateResolver.html
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanBinding {
    /// Digest over what was judged: the rendered plan for a statement gate,
    /// the operation and its resource references for every other gate.
    pub plan_digest: PlanDigest,
    /// The working directory it was judged in, as a logical path — the
    /// spelling the VFS router resolves against, never a host path.
    pub cwd: String,
    /// The scope it was judged in.
    pub scope: ApprovalScope,
    /// Which sandbox profile was in force, when the embedder names them.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sandbox_profile: Option<SandboxProfileId>,
}

impl PlanBinding {
    /// Bind a decision to a digest, a working directory, and a scope.
    pub fn new(plan_digest: PlanDigest, cwd: impl Into<String>, scope: ApprovalScope) -> Self {
        Self {
            plan_digest,
            cwd: cwd.into(),
            scope,
            sandbox_profile: None,
        }
    }

    /// Name the sandbox profile in force.
    pub fn with_sandbox_profile(mut self, profile: SandboxProfileId) -> Self {
        self.sandbox_profile = Some(profile);
        self
    }

    /// How `self` (the binding a redemption presents) differs from `approved`
    /// (the binding the grant was decided against), or `None` when they
    /// agree.
    ///
    /// Names the first difference and both values, because "this replay is
    /// not what was approved" is only actionable if it says how.
    pub fn mismatch(&self, approved: &Self) -> Option<String> {
        if self.cwd != approved.cwd {
            return Some(format!(
                "it runs in {} where {} was approved",
                self.cwd, approved.cwd
            ));
        }
        if self.scope != approved.scope {
            return Some(format!(
                "it runs under {} where {} was approved",
                self.scope, approved.scope
            ));
        }
        if self.sandbox_profile != approved.sandbox_profile {
            return Some(format!(
                "it runs under sandbox profile {} where {} was approved",
                render_profile(self.sandbox_profile.as_ref()),
                render_profile(approved.sandbox_profile.as_ref()),
            ));
        }
        if self.plan_digest != approved.plan_digest {
            return Some(format!(
                "it digests to {} where {} was approved",
                self.plan_digest, approved.plan_digest
            ));
        }
        None
    }
}

/// One sandbox profile, for a diagnostic an operator reads.
fn render_profile(profile: Option<&SandboxProfileId>) -> String {
    match profile {
        Some(profile) => profile.to_string(),
        None => "none".to_string(),
    }
}

// ───────────────────────── Risk, resources, transitions ─────────────────────────

/// How hard a request is to walk back. Read by an approver and matched by
/// policy — it carries no redemption semantics of its own (spec §F.3).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RiskClass {
    /// Trivially undoable.
    Reversible,
    /// Undoable with effort (e.g. the trash, a git revert).
    Recoverable,
    /// Not undoable.
    Irreversible,
}

/// A resource identity that is more than a path: a namespaced kind plus id,
/// and the state-transition claim being authorized, when there is one
/// (spec §A.3).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Resource {
    /// Namespace of the identifier. In-tree: `"path"`. Plugin-registered:
    /// `"git.ref"`, `"git.remote"`, `"git.worktree"`, `"url"`, `"job"`.
    pub kind: String,
    /// Identifier within that namespace (`"/home/a/x.txt"`,
    /// `"refs/heads/main"`, `"origin"`).
    pub id: String,
    /// The state-transition claim being authorized, when there is one.
    pub transition: Option<Transition>,
}

impl Resource {
    /// A resource with a declared before/after state claim.
    pub fn transition(
        kind: impl Into<String>,
        id: impl Into<String>,
        from: StateClaim,
        to: StateClaim,
    ) -> Self {
        Self {
            kind: kind.into(),
            id: id.into(),
            transition: Some(Transition { from, to }),
        }
    }

    /// A resource with no transition claim (e.g. `git.remote: origin`).
    pub fn plain(kind: impl Into<String>, id: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            id: id.into(),
            transition: None,
        }
    }

    /// Project to the bare `(kind, id)` pair, dropping the transition claim.
    pub fn to_ref(&self) -> ResourceRef {
        ResourceRef {
            kind: self.kind.clone(),
            id: self.id.clone(),
        }
    }

    /// The redemption-time [`Condition`] this resource implies: "the world
    /// must still show `transition.from`". `None` when the resource declared
    /// no transition — nothing to re-check at redemption.
    pub fn to_condition(&self) -> Option<Condition> {
        self.transition.as_ref().map(|t| Condition {
            resource: self.to_ref(),
            expected_from: t.from.clone(),
        })
    }
}

/// Names a resource without its transition claim — the pair an
/// [`Observation`] or a match result points at.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResourceRef {
    /// Namespace of the identifier (see [`Resource::kind`]).
    pub kind: String,
    /// Identifier within that namespace (see [`Resource::id`]).
    pub id: String,
}

/// A before/after state claim on one [`Resource`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Transition {
    /// The claimed state before the operation.
    pub from: StateClaim,
    /// The claimed state after the operation.
    pub to: StateClaim,
}

/// One side of a [`Transition`]: what a resource's state is claimed to be.
///
/// `Unspecified` is a distinct variant, not a wildcard: it never compares
/// equal to any concrete claim (`Absent`/`Exact`/`Digest`), including
/// another `Unspecified` compared against a concrete one. It only equals
/// itself. This is ordinary derived enum equality — the point is that no
/// custom `PartialEq` ever gives it wildcard semantics (spec §A.3).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StateClaim {
    /// The resource does not exist (pre: creating; post: deleting).
    Absent,
    /// An opaque identifier the producer will re-derive at redemption: a git
    /// oid, an etag, a generation number.
    Exact(String),
    /// A content digest.
    Digest {
        /// The digest algorithm (e.g. `"sha256"`).
        alg: String,
        /// The digest, hex-encoded.
        hex: String,
    },
    /// "I don't claim anything about this side." Legal, but a grant whose
    /// conditions are all `Unspecified` records that fact so an auditor can
    /// see which approvals were unconditioned.
    Unspecified,
}

// ───────────────────────── The statement plan ─────────────────────────

/// What one top-level statement was asked to run (spec §C.6).
///
/// Built from the AST after validation and **before** execution, so it is
/// parse information and never execution information: no substitution has
/// run, no redirect has been opened, no loop has taken its first iteration.
/// Nested statements — loop bodies, `if` branches, user-tool bodies — belong
/// to their enclosing top-level statement's plan and are never planned
/// separately.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Plan {
    /// The statement rendered back to shell text, **unexpanded**: `${HOME}`
    /// and `$(...)` appear as written, because a classifier judges what was
    /// asked, not what it resolved to. Truncated at
    /// [`PLAN_RENDER_LIMIT`] bytes with a marker naming the limit.
    pub rendered: String,
    /// The statement's kind: `"command"`, `"pipeline"`, `"for"`,
    /// `"and_chain"`, …
    pub statement_kind: String,
    /// Every command the statement contains, control-structure bodies
    /// included.
    pub commands: Vec<PlannedCommand>,
}

/// The byte limit [`Plan::rendered`] is truncated at: 8 KiB. A statement
/// longer than this is a generated program, and a classifier that needs more
/// than 8 KiB of it is reading the wrong field — [`Plan::commands`] carries
/// the structure.
pub const PLAN_RENDER_LIMIT: usize = 8 * 1024;

impl Plan {
    /// Assemble a plan. The only constructor for this `#[non_exhaustive]`
    /// type — `rendered` is stored verbatim, so a producer truncates before
    /// calling.
    pub fn new(
        rendered: impl Into<String>,
        statement_kind: impl Into<String>,
        commands: Vec<PlannedCommand>,
    ) -> Self {
        Self {
            rendered: rendered.into(),
            statement_kind: statement_kind.into(),
            commands,
        }
    }
}

/// One command inside a [`Plan`], as written (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlannedCommand {
    /// argv0 as written — never resolved through aliases, `PATH`, or the
    /// tool registry.
    pub name: String,
    /// The arguments, rendered unexpanded.
    pub args: Vec<String>,
    /// The redirections this command declares.
    pub redirects: Vec<PlannedRedirect>,
    /// Whether the enclosing pipeline was backgrounded with `&`.
    pub background: bool,
}

impl PlannedCommand {
    /// Name one planned command. The only constructor for this
    /// `#[non_exhaustive]` type.
    pub fn new(
        name: impl Into<String>,
        args: Vec<String>,
        redirects: Vec<PlannedRedirect>,
        background: bool,
    ) -> Self {
        Self {
            name: name.into(),
            args,
            redirects,
            background,
        }
    }
}

/// One redirection inside a [`PlannedCommand`] (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlannedRedirect {
    /// The operator as written: `">"`, `">>"`, `"2>"`, `"<"`, `"<<<"`, …
    pub kind: String,
    /// The target, rendered unexpanded — `> ${LOG}` keeps `${LOG}`.
    pub target: String,
}

impl PlannedRedirect {
    /// Name one planned redirection. The only constructor for this
    /// `#[non_exhaustive]` type.
    pub fn new(kind: impl Into<String>, target: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            target: target.into(),
        }
    }
}

// ───────────────────────── Capture ─────────────────────────

/// The exact captured invocation of a gated tool call: the argv the approval
/// side would replay.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Invocation {
    /// The dispatch name of the gated tool.
    pub tool: String,
    /// The captured argv.
    pub argv: Vec<String>,
}

/// Whether this invocation can be replayed by the approval side, and why not
/// when it cannot. Never a silently-empty argv (spec §B.4).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Capture {
    /// Replayable by the approval side.
    Exact(Invocation),
    /// A direct `tool.execute` with no dispatch seam above it (a unit test).
    DirectExecution,
    /// The invocation cannot be represented as argv without loss.
    Unavailable {
        /// Why capture is not possible for this call shape.
        reason: String,
    },
    /// Capture was attempted and failed.
    CaptureFailed {
        /// What went wrong while capturing.
        reason: String,
    },
    /// A statement gate (spec §C.6): the whole program source plus the index
    /// of the held top-level statement. Replayable — `Kernel::confirm`
    /// re-parses the source and executes exactly statement `index`, in the
    /// originating session, where earlier statements' effects (variables,
    /// cwd) are session state and still hold. Statements carry no source
    /// spans, which is why the capture is source-plus-index rather than a
    /// slice.
    Statement {
        /// The program source the top-level loop parsed.
        source: String,
        /// Which top-level statement of that source was held, counting
        /// from 0 over every statement the parse produced.
        index: usize,
    },
}

// ───────────────────────── Request context (tracing) ─────────────────────────

/// W3C trace context captured at request time, so an approval granted long
/// after the request still nests under the originating trace (spec §A.3,
/// §G).
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RequestContext {
    /// W3C `traceparent`, if one was live at request time.
    pub traceparent: Option<String>,
    /// W3C `tracestate`, if one was live at request time.
    pub tracestate: Option<String>,
    /// The baggage subset captured at request time.
    pub baggage: BTreeMap<String, String>,
}

// ───────────────────────── ApprovalRequest + builder ─────────────────────────

/// The request entry: one privileged operation asking to proceed (spec
/// §A.3). Posted by the implementation side; every field except the ones a
/// producer supplies through [`ApprovalRequest::builder`] is stamped by the
/// kernel (`id`, `principal`, `capture`, `context`, `requested_at`,
/// `deadline`, `job_id`) — see [`ApprovalRequestDraft::stamp`].
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ApprovalRequest {
    /// The request's public name.
    pub id: RequestId,
    /// Which kernel, session, and actor this request belongs to (spec §A.7).
    /// Mandatory: a helper hosting several sessions must never need an
    /// external map to answer "whose request is this?".
    pub scope: ApprovalScope,
    /// Set when this request was raised while another was already being
    /// satisfied — a statement gate that reaches an `fs.*` gate underneath it
    /// (spec §A.7). Lets a UI show one nested prompt instead of two unrelated
    /// ones. A grant on a parent never implies authority for a child.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parent: Option<RequestId>,
    /// Bumped on every recorded transition of this request. A decision that
    /// quotes a stale revision is refused, not applied (spec §B.6) — the
    /// checks themselves land with the late-answer rule; this field is what
    /// they quote.
    #[serde(default)]
    pub revision: u64,
    /// The context this request was raised in, and the context a redemption
    /// must still match (spec §A.9).
    pub binding: PlanBinding,
    /// Dotted taxonomy (`"fs.remove"`, `"trash.empty"`, `"git.push"`).
    pub operation: OperationId,
    /// How hard this operation is to walk back.
    pub risk: RiskClass,
    /// The resources this operation would touch.
    pub resources: Vec<Resource>,
    /// Who is asking.
    pub principal: Principal,
    /// Whether this invocation can be replayed by the approval side.
    pub capture: Capture,
    /// W3C context captured at request time.
    pub context: RequestContext,
    /// The backgrounded job that raised this request, if any.
    pub job_id: Option<u64>,
    /// Why the gate fired.
    pub reason: String,
    /// Display-only re-run template. Producer-authored, therefore untrusted
    /// text (spec §C.3) — never contains a credential.
    pub hint: String,
    /// The clock reading this entry was committed at.
    #[serde(with = "crate::rfc3339::system_time")]
    pub requested_at: SystemTime,
    /// When this request stops being answerable. `None` — the default —
    /// means it never does: it lives until decided or cancelled (spec
    /// §A.10). The kernel never enforces this field on a timer; it is
    /// compared when the request is next observed, exactly like a grant's
    /// `not_after`. An embedder that wants deadlines sets them and cancels
    /// what it no longer wants (spec §B.5).
    #[serde(default, with = "crate::rfc3339::opt_system_time")]
    pub deadline: Option<SystemTime>,
    /// Set when this request replaces a closed predecessor — the operation
    /// was cancelled, or denied, and asked again (spec §B.5).
    pub supersedes: Option<RequestId>,
    /// The parsed statement plan, present exactly when the operation is the
    /// statement gate (spec §C.6). Typed here and mirrored as `cmd`
    /// resources, so a classifier reads structure and a standing grant
    /// matches globs.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan: Option<Plan>,
}

impl ApprovalRequest {
    /// Start building a draft request for `operation` (a dotted id, e.g.
    /// `"git.push"`). The draft carries no principal, capture, id, context,
    /// or timing — those are kernel-stamped (spec §D.1).
    pub fn builder(operation: impl Into<String>) -> ApprovalRequestBuilder {
        REQUESTS_CONSTRUCTED.fetch_add(1, Ordering::Relaxed);
        ApprovalRequestBuilder {
            operation: operation.into(),
            risk: None,
            resources: Vec::new(),
            reason: String::new(),
            hint: String::new(),
            supersedes: None,
            plan: None,
        }
    }

    /// How many approval requests this process has begun building, ever —
    /// one per [`ApprovalRequest::builder`] call, counted whether or not the
    /// draft is ever built, stamped, or posted.
    ///
    /// A test asserts on this. An `fs.*` operation nothing is subscribed to
    /// must build **no** request at all, however many paths it names, and a
    /// counter is the only way to state that as a number: read it either
    /// side of a 10,000-path `rm -rf` and the difference must be 0. Relaxed
    /// ordering, because it is a process-wide diagnostic total and never a
    /// synchronization point — read it from one task at a time, or accept
    /// that a concurrent builder may or may not be counted yet.
    pub fn constructed_count() -> u64 {
        REQUESTS_CONSTRUCTED.load(Ordering::Relaxed)
    }
}

/// Backing store for [`ApprovalRequest::constructed_count`].
static REQUESTS_CONSTRUCTED: AtomicU64 = AtomicU64::new(0);

/// The public view of a request: everything in [`ApprovalRequest`] and
/// nothing else — deliberately no credential field, so there is nothing to
/// redact and nothing to leak through clone/serde/VFS/telemetry (spec §A.2).
/// This is what `ExecResult.approval`, `JobInfo.approval`, `/v/approvals`,
/// and a `Policy`'s input all see.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ApprovalRequestView {
    /// See [`ApprovalRequest::id`].
    pub id: RequestId,
    /// See [`ApprovalRequest::scope`].
    pub scope: ApprovalScope,
    /// See [`ApprovalRequest::parent`].
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parent: Option<RequestId>,
    /// See [`ApprovalRequest::revision`]. This is the revision a decision on
    /// this request must quote (spec §B.6).
    #[serde(default)]
    pub revision: u64,
    /// See [`ApprovalRequest::binding`].
    pub binding: PlanBinding,
    /// See [`ApprovalRequest::operation`].
    pub operation: OperationId,
    /// See [`ApprovalRequest::risk`].
    pub risk: RiskClass,
    /// See [`ApprovalRequest::resources`].
    pub resources: Vec<Resource>,
    /// See [`ApprovalRequest::principal`].
    pub principal: Principal,
    /// See [`ApprovalRequest::capture`].
    pub capture: Capture,
    /// See [`ApprovalRequest::context`].
    pub context: RequestContext,
    /// See [`ApprovalRequest::job_id`].
    pub job_id: Option<u64>,
    /// See [`ApprovalRequest::reason`].
    pub reason: String,
    /// See [`ApprovalRequest::hint`].
    pub hint: String,
    /// See [`ApprovalRequest::requested_at`].
    #[serde(with = "crate::rfc3339::system_time")]
    pub requested_at: SystemTime,
    /// See [`ApprovalRequest::deadline`].
    #[serde(default, with = "crate::rfc3339::opt_system_time")]
    pub deadline: Option<SystemTime>,
    /// See [`ApprovalRequest::supersedes`].
    pub supersedes: Option<RequestId>,
    /// See [`ApprovalRequest::plan`]. An approver reads this to see what the
    /// statement was asked to run.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan: Option<Plan>,
}

impl From<ApprovalRequest> for ApprovalRequestView {
    fn from(req: ApprovalRequest) -> Self {
        Self {
            id: req.id,
            scope: req.scope,
            parent: req.parent,
            revision: req.revision,
            binding: req.binding,
            operation: req.operation,
            risk: req.risk,
            resources: req.resources,
            principal: req.principal,
            capture: req.capture,
            context: req.context,
            job_id: req.job_id,
            reason: req.reason,
            hint: req.hint,
            requested_at: req.requested_at,
            deadline: req.deadline,
            supersedes: req.supersedes,
            plan: req.plan,
        }
    }
}

impl From<&ApprovalRequest> for ApprovalRequestView {
    fn from(req: &ApprovalRequest) -> Self {
        req.clone().into()
    }
}

/// A producer-built draft: everything a caller supplies through
/// [`ApprovalRequest::builder`], and nothing the kernel must stamp. There is
/// no `principal` field, no `capture` field, no `id` field, no `context`
/// field, and no `requested_at` field — a plugin cannot forge a principal or
/// an invocation because the type has nowhere to put one (spec §D.1).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ApprovalRequestDraft {
    /// Dotted taxonomy.
    pub operation: OperationId,
    /// How hard this operation is to walk back.
    pub risk: RiskClass,
    /// The resources this operation would touch.
    pub resources: Vec<Resource>,
    /// Why the gate fired.
    pub reason: String,
    /// Display-only re-run template.
    pub hint: String,
    /// Set when this request replaces a closed predecessor (spec §B.5).
    pub supersedes: Option<RequestId>,
    /// The parsed statement plan, for the statement gate (spec §C.6). The
    /// plan is producer information — it comes from the AST the gate site
    /// holds — so it rides on the draft rather than being kernel-stamped.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan: Option<Plan>,
}

/// Everything the kernel stamps onto a draft that a producer cannot supply
/// (spec §D.1) — the identity, the scope, the binding, and the provenance a
/// plugin must not be able to forge.
///
/// One struct rather than a nine-argument [`ApprovalRequestDraft::stamp`]:
/// three of these are optional and two are ids, so positional arguments were
/// one reordering away from stamping a parent as a supersession.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub struct RequestOrigin {
    /// Which kernel, session, and actor raised it (spec §A.7).
    pub scope: ApprovalScope,
    /// The request this one was raised underneath, if any (spec §A.7).
    pub parent: Option<RequestId>,
    /// The context this request must still be redeemed in (spec §A.9).
    pub binding: PlanBinding,
    /// Who is asking.
    pub principal: Principal,
    /// Whether this invocation can be replayed by the approval side.
    pub capture: Capture,
    /// W3C context captured at request time.
    pub context: RequestContext,
    /// When the request stops being answerable, when the embedder set one
    /// (spec §A.10). `None` is the default and means it never does.
    pub deadline: Option<SystemTime>,
    /// The backgrounded job that raised it, if any.
    pub job_id: Option<u64>,
}

impl RequestOrigin {
    /// The four values every request must have. `context`, `parent`,
    /// `deadline`, and `job_id` default to absent — add them with the
    /// `with_*` methods.
    pub fn new(
        scope: ApprovalScope,
        binding: PlanBinding,
        principal: Principal,
        capture: Capture,
    ) -> Self {
        Self {
            scope,
            parent: None,
            binding,
            principal,
            capture,
            context: RequestContext::default(),
            deadline: None,
            job_id: None,
        }
    }

    /// Name the request this one was raised underneath (spec §A.7).
    pub fn with_parent(mut self, parent: Option<RequestId>) -> Self {
        self.parent = parent;
        self
    }

    /// Attach the W3C trace context captured at request time.
    pub fn with_context(mut self, context: RequestContext) -> Self {
        self.context = context;
        self
    }

    /// Name the backgrounded job that raised it.
    pub fn with_job_id(mut self, job_id: Option<u64>) -> Self {
        self.job_id = job_id;
        self
    }

    /// Set when this request stops being answerable (spec §A.10). The
    /// kernel records the value and compares it when the request is
    /// observed; it runs no timer and cancels nothing on its own. An
    /// embedder that wants a horizon sets one here and calls
    /// `Requester::cancel` when it passes.
    pub fn with_deadline(mut self, deadline: Option<SystemTime>) -> Self {
        self.deadline = deadline;
        self
    }
}

impl ApprovalRequestDraft {
    /// Stamp the kernel-supplied fields, turning a draft into a postable
    /// [`ApprovalRequest`]. Pure field assembly — no I/O, no validation
    /// beyond what the draft already carries. This method is `pub` and does
    /// not itself gate who calls it: the guarantee is that a *draft* cannot
    /// carry these fields, and the kernel's `request_approval` seam is the
    /// one place real values enter.
    ///
    /// `revision` starts at 0 and is bumped by the ledger on every recorded
    /// transition (spec §A.7) — a draft cannot set it either.
    pub fn stamp(
        self,
        id: RequestId,
        requested_at: SystemTime,
        origin: RequestOrigin,
    ) -> ApprovalRequest {
        ApprovalRequest {
            id,
            scope: origin.scope,
            parent: origin.parent,
            revision: 0,
            binding: origin.binding,
            operation: self.operation,
            risk: self.risk,
            resources: self.resources,
            principal: origin.principal,
            capture: origin.capture,
            context: origin.context,
            job_id: origin.job_id,
            reason: self.reason,
            hint: self.hint,
            requested_at,
            deadline: origin.deadline,
            supersedes: self.supersedes,
            plan: self.plan,
        }
    }
}

/// Builder for [`ApprovalRequestDraft`]. See [`ApprovalRequest::builder`].
#[derive(Debug, Clone)]
pub struct ApprovalRequestBuilder {
    operation: String,
    risk: Option<RiskClass>,
    resources: Vec<Resource>,
    reason: String,
    hint: String,
    supersedes: Option<RequestId>,
    plan: Option<Plan>,
}

/// Why an [`ApprovalRequestBuilder::build`] call failed.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ApprovalRequestBuildError {
    /// `operation` was empty — an unnamed operation cannot be judged, and
    /// the ledger's taxonomy check depends on a real dotted id (spec §A.6).
    #[error("approval request operation must not be empty")]
    EmptyOperation,
    /// `risk` was never set. There is no safe default: silently picking
    /// `Reversible` could downgrade an irreversible operation past a policy
    /// that keys on risk class.
    #[error("approval request risk class must be set explicitly — there is no safe default")]
    MissingRisk,
}

impl ApprovalRequestBuilder {
    /// Set the risk class. Required — [`Self::build`] fails without it.
    pub fn risk(mut self, risk: RiskClass) -> Self {
        self.risk = Some(risk);
        self
    }

    /// Add one resource this operation would touch.
    pub fn resource(mut self, resource: Resource) -> Self {
        self.resources.push(resource);
        self
    }

    /// Set why the gate fired.
    pub fn reason(mut self, reason: impl Into<String>) -> Self {
        self.reason = reason.into();
        self
    }

    /// Set the display-only re-run hint.
    pub fn hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = hint.into();
        self
    }

    /// Mark this draft as replacing a closed predecessor (spec §B.5) — the
    /// operation was cancelled, or denied, and is being asked again.
    pub fn supersedes(mut self, id: RequestId) -> Self {
        self.supersedes = Some(id);
        self
    }

    /// Attach the parsed statement plan (spec §C.6). Set by the statement
    /// gate and by nothing else — an `fs.*` request carries no plan.
    pub fn plan(mut self, plan: Plan) -> Self {
        self.plan = Some(plan);
        self
    }

    /// Finish the draft. Fails when `operation` is empty or `risk` was never
    /// set.
    pub fn build(self) -> Result<ApprovalRequestDraft, ApprovalRequestBuildError> {
        let operation =
            OperationId::new(self.operation).map_err(|_| ApprovalRequestBuildError::EmptyOperation)?;
        let risk = self.risk.ok_or(ApprovalRequestBuildError::MissingRisk)?;
        Ok(ApprovalRequestDraft {
            operation,
            risk,
            resources: self.resources,
            reason: self.reason,
            hint: self.hint,
            supersedes: self.supersedes,
            plan: self.plan,
        })
    }
}

// ───────────────────────── Grant side ─────────────────────────

/// The authorization entry: one decision to allow a request (spec §A.4).
/// Posted by the approval side. There is no redemption-limit field — see the
/// comment on [`GrantTerms`] for why.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Grant {
    /// The request this grant authorizes.
    pub request: RequestId,
    /// Who decided.
    pub decided_by: Principal,
    /// Why the decision was made, and by which mechanism.
    pub grounds: Grounds,
    /// The grant expires at this time if unredeemed.
    #[serde(with = "crate::rfc3339::system_time")]
    pub not_after: SystemTime,
    /// First 4 hex characters of the credential, for correlating a
    /// `TokenRejected` with the grant it was aimed at. The credential itself
    /// is never in an entry (spec §A.2).
    pub token_prefix: String,
    // No redemption-limit field. Every grant authorizes exactly one
    // successful settlement; failed attempts do not consume it (spec §A.1).
    // A rule that should fire repeatedly is a StandingGrant with `max_uses`
    // (spec §C.4).
    /// Preconditions re-verified at redemption. Defaults to exactly the
    /// transitions declared on the request's resources. An approver may
    /// narrow (add or tighten) and may never widen — enforced at post time.
    pub conditions: Vec<Condition>,
    /// The clock reading the decision was committed at.
    #[serde(with = "crate::rfc3339::system_time")]
    pub decided_at: SystemTime,
}

impl Grant {
    /// Build a `Grant` from its terms plus the decision provenance. The only
    /// constructor this `#[non_exhaustive]` type has outside this crate —
    /// `token_prefix` is computed by the caller from the freshly minted
    /// [`Token`] (never stored here — spec §A.2), so it is threaded through
    /// rather than derived from anything already on `terms`.
    pub fn from_terms(
        request: RequestId,
        decided_by: Principal,
        grounds: Grounds,
        terms: GrantTerms,
        token_prefix: String,
        decided_at: SystemTime,
    ) -> Self {
        Self {
            request,
            decided_by,
            grounds,
            not_after: terms.not_after,
            token_prefix,
            conditions: terms.conditions,
            decided_at,
        }
    }
}

/// The terms an [`Decision::Grant`] carries before the kernel turns them
/// into a full [`Grant`] (which also records `request`, `decided_by`, and
/// `decided_at`).
///
/// No redemption-count field here either, for the same reason as `Grant`:
/// single-successful-redemption is the rule, not a configurable limit.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GrantTerms {
    /// The grant expires at this time if unredeemed.
    #[serde(with = "crate::rfc3339::system_time")]
    pub not_after: SystemTime,
    /// Preconditions re-verified at redemption.
    pub conditions: Vec<Condition>,
}

impl GrantTerms {
    /// A one-shot grant good until `not_after`, with conditions defaulted to
    /// exactly the transitions the request declared on its resources (spec
    /// §A.4).
    pub fn once_for(req: &ApprovalRequest, not_after: SystemTime) -> Self {
        let conditions = req.resources.iter().filter_map(Resource::to_condition).collect();
        Self { not_after, conditions }
    }

    /// The same terms, from the tokenless [`ApprovalRequestView`] an
    /// approver actually holds.
    ///
    /// **This is what an approver should call.** An approver never sees the
    /// stamped [`ApprovalRequest`], and rebuilding one from a view to reach
    /// [`Self::once_for`] drops the request's resources unless the caller
    /// remembers to copy them one by one — which produces terms with no
    /// conditions, and the ledger rejects those as widening (spec §A.4).
    pub fn once_for_view(view: &ApprovalRequestView, not_after: SystemTime) -> Self {
        let conditions = view.resources.iter().filter_map(Resource::to_condition).collect();
        Self { not_after, conditions }
    }

    /// Build terms directly from an explicit condition list. The only other
    /// external constructor for this `#[non_exhaustive]` type besides
    /// [`Self::once_for`] and [`Self::once_for_view`] — an approver that
    /// narrows (adds or tightens) beyond the request's declared transitions,
    /// rather than accepting them verbatim, needs this rather than a struct
    /// literal.
    pub fn new(not_after: SystemTime, conditions: Vec<Condition>) -> Self {
        Self { not_after, conditions }
    }
}

/// Why a request was granted, and by which mechanism (spec §A.4).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Grounds {
    /// A human said yes.
    Human {
        /// Distinguishes the REPL terminal from an embedder's out-of-band UI.
        channel: String,
    },
    /// The embedder's synchronous policy hook.
    Policy {
        /// The rule that matched.
        rule: String,
    },
    /// A standing grant already in the ledger fired.
    Standing {
        /// The standing grant that produced this decision.
        grant: StandingId,
    },
    /// The embedder granted directly through its `ApproverHandle`.
    Embedder,
}

/// A precondition re-verified at redemption: "the world must still show
/// `expected_from` for `resource`" (spec §B.4).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Condition {
    /// The resource this condition constrains.
    pub resource: ResourceRef,
    /// The state the resource must still be in.
    pub expected_from: StateClaim,
}

/// What a redemption-time condition check saw, and when (spec §A.5).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Observation {
    /// The resource observed.
    pub resource: ResourceRef,
    /// What was observed.
    pub claim: StateClaim,
    /// When it was observed.
    #[serde(with = "crate::rfc3339::system_time")]
    pub at: SystemTime,
}

/// A rule that auto-grants matching future requests. Itself a ledger entry
/// (`StandingIssued`) — every request it auto-approves produces a normal
/// `Granted` entry naming it (spec §C.4).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StandingGrant {
    /// This standing grant's id.
    pub id: StandingId,
    /// Operation patterns this rule covers (e.g. `"git.commit"`, `"fs.*"`).
    pub operations: Vec<OperationPattern>,
    /// Resource patterns this rule covers.
    pub resources: Vec<ResourcePattern>,
    /// Restrict to one requesting principal; `None` means any requester in
    /// this session.
    pub principal: Option<Principal>,
    /// Maximum number of matching requests this rule may auto-approve.
    /// Defaults to 1 — a standing rule is one-shot unless explicitly
    /// widened ([`Self::with_max_uses`] / [`Self::unlimited_uses`]).
    /// `None` is explicit unlimited; an omitted field on the wire is the
    /// one-shot default, never unlimited. **On the wire, `"max_uses":
    /// null` reads as explicit unlimited** — a producer that means "use
    /// the default" must omit the field, not send null.
    #[serde(default = "default_max_uses")]
    pub max_uses: Option<u32>,
    /// When this rule stops matching, if it expires.
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "crate::rfc3339::opt_system_time"
    )]
    pub expires_at: Option<SystemTime>,
    /// Who issued this rule.
    pub issued_by: Principal,
    /// Why this rule exists.
    pub reason: String,
}

/// The wire and constructor default for [`StandingGrant::max_uses`]:
/// one-shot. Wider is always an explicit act.
fn default_max_uses() -> Option<u32> {
    Some(1)
}

impl StandingGrant {
    /// Build a not-yet-issued standing grant, one-shot by default
    /// (`max_uses = Some(1)`) — widen explicitly with
    /// [`Self::with_max_uses`] or [`Self::unlimited_uses`]. `id` is a
    /// placeholder — `ApproverHandle::grant_standing` overwrites it with a
    /// ledger-allocated [`StandingId`] when the rule is issued (spec §C.4);
    /// there is no separate draft type here for the same reason
    /// [`ApprovalRequestDraft`] exists for [`ApprovalRequest`]. The only
    /// external constructor for this `#[non_exhaustive]` type.
    pub fn new(
        operations: Vec<OperationPattern>,
        resources: Vec<ResourcePattern>,
        principal: Option<Principal>,
        expires_at: Option<SystemTime>,
        issued_by: Principal,
        reason: impl Into<String>,
    ) -> Self {
        Self {
            id: StandingId::new(0),
            operations,
            resources,
            principal,
            max_uses: default_max_uses(),
            expires_at,
            issued_by,
            reason: reason.into(),
        }
    }

    /// Widen the rule to auto-approve up to `n` matching requests.
    pub fn with_max_uses(mut self, n: u32) -> Self {
        self.max_uses = Some(n);
        self
    }

    /// Remove the use bound entirely. Unlimited is an explicit act, never
    /// a default — say so in `reason`.
    pub fn unlimited_uses(mut self) -> Self {
        self.max_uses = None;
        self
    }
}

/// A resource-matching pattern (`{ kind: "git.ref", pattern:
/// "refs/heads/agent/*" }`). Pattern *data* only — matching is
/// `kaish-glob`'s job (kaish-types must not depend on it); kind must match
/// exactly, only `id` globs (spec §C.4).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResourcePattern {
    /// Namespace of the identifier (see [`Resource::kind`]) — matched
    /// exactly, never globbed.
    pub kind: String,
    /// A glob pattern over [`Resource::id`].
    pub pattern: String,
}

impl ResourcePattern {
    /// Build a resource pattern.
    pub fn new(kind: impl Into<String>, pattern: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            pattern: pattern.into(),
        }
    }
}

/// A glob-scoped registration making matching operations `observe` (record
/// only) or `enforce` (decide).
///
/// Registering one appends a `Subscribed` entry, and revoking one appends
/// `Unsubscribed` — an audit record whose own scope changed with no record
/// of the change would be unreadable.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Subscription {
    /// This subscription's id.
    pub id: SubscriptionId,
    /// Operation patterns this subscription covers.
    pub operations: Vec<OperationPattern>,
    /// Resource patterns this subscription covers.
    pub resources: Vec<ResourcePattern>,
    /// Whether matching operations are recorded only, or go through the
    /// real decision chain.
    pub mode: SubscriptionMode,
    /// Why this subscription exists.
    pub reason: String,
}

impl Subscription {
    /// Build a not-yet-registered subscription. `id` is a placeholder:
    /// `ApproverHandle::subscribe` overwrites it with a ledger-allocated
    /// [`SubscriptionId`] and returns the authoritative one, the same shape
    /// [`StandingGrant::new`] has. This is the only constructor outside the
    /// crate — the type is `#[non_exhaustive]`.
    pub fn new(
        operations: Vec<OperationPattern>,
        resources: Vec<ResourcePattern>,
        mode: SubscriptionMode,
        reason: impl Into<String>,
    ) -> Self {
        Self {
            id: SubscriptionId::new(0),
            operations,
            resources,
            mode,
            reason: reason.into(),
        }
    }
}

/// The two subscription modes — record, or decide.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SubscriptionMode {
    /// Matching operations post one `Observed` entry and proceed; they never
    /// defer, never block, never prompt. A record, not a decision — no
    /// request is built and no grant exists to redeem.
    Observe,
    /// Matching operations go through the decision chain, and may defer.
    Enforce,
}

/// One resource an `Observed` entry records (spec §C.5, §C.6).
///
/// Carries both spellings of the path deliberately: the subscription's glob
/// matched `resolved` — a scope a relative path could step outside of is not
/// a scope — while `id` keeps the string the command named, because that is
/// what an auditor reading the log recognizes.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ObservedResource {
    /// Namespace of the identifier — `"path"` for the kernel's fs gates,
    /// `"cmd"` for the statement tap.
    pub kind: String,
    /// What the command named.
    pub id: String,
    /// The resolved form the subscription's glob matched. Equal to `id` when
    /// nothing resolved it.
    pub resolved: String,
    /// The subscription that covered it, or `None` when no subscription did.
    /// The statement tap (spec §C.6) is always `None`: `cmd.*` never enters
    /// the subscription registry, because the classifier is the sole posture
    /// decider for statements and two deciders could disagree.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub subscription: Option<SubscriptionId>,
}

impl ObservedResource {
    /// Name one resource a subscription covered (spec §C.5).
    pub fn new(
        kind: impl Into<String>,
        id: impl Into<String>,
        resolved: impl Into<String>,
        subscription: SubscriptionId,
    ) -> Self {
        Self {
            kind: kind.into(),
            id: id.into(),
            resolved: resolved.into(),
            subscription: Some(subscription),
        }
    }

    /// Name one resource the statement tap recorded (spec §C.6): no
    /// subscription covered it, and nothing resolved its id.
    pub fn planned(kind: impl Into<String>, id: impl Into<String>) -> Self {
        let id = id.into();
        Self {
            kind: kind.into(),
            resolved: id.clone(),
            id,
            subscription: None,
        }
    }
}

/// A policy's verdict on a request (spec §C.2).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Decision {
    /// Grant, on these terms.
    Grant(GrantTerms),
    /// Deny, with a reason.
    Deny {
        /// Why the request was denied.
        reason: String,
    },
    /// "Not my call." Falls through to the next decision-chain stage. Never
    /// means "yes".
    Defer,
}

// ───────────────────────── Attempt outcome ─────────────────────────

/// How a redeemed attempt ended (spec §A.5).
///
/// Externally tagged (`{"exit":0}`, `{"unknown":{"cause":"cancelled"}}`) —
/// unlike [`Grounds`] or [`LedgerEntry`], `Exit`/`Error` wrap a bare scalar
/// rather than a struct, and serde cannot represent a scalar-wrapping
/// newtype variant under internal tagging.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Outcome {
    /// The tool ran and reported a POSIX exit code.
    Exit(i64),
    /// The tool ran and reported an error.
    Error(String),
    /// The attempt's executor went away before reporting an exit code. The
    /// operation may already have taken effect — this outcome never means
    /// "nothing happened", which is why there is no `Cancelled` variant.
    Unknown {
        /// Why the executor is presumed lost.
        cause: LostCause,
    },
}

/// Why an attempt's executor is presumed lost (spec §A.5).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LostCause {
    /// The dispatcher's `AttemptGuard` was dropped (cooperative
    /// cancellation, a panic, an aborted task).
    Cancelled,
    /// The recovery sweep found a reservation nobody reported on.
    ExecutorLost,
}

/// The three states of one attempt (spec §B.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AttemptState {
    /// Reservation committed; no terminal report yet.
    Reserved,
    /// Something reported: an exit code, an error, or `Outcome::Unknown`.
    Settled,
    /// The recovery sweep found a reservation nobody reported on.
    Abandoned,
}

/// The top-level state of one request (spec §B.2's request state machine).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RequestState {
    /// Posted; awaiting a decision.
    Requested,
    /// Decided yes. May still be reserving/settling attempts.
    Granted,
    /// Decided no.
    Denied,
    /// Closed from the requesting side with no decision (spec §B.5). Asking
    /// again posts a **new** request linked by `supersedes`.
    Cancelled,
    /// A deadline the embedder set elapsed, or a grant's `not_after` did.
    /// There is no default deadline — nothing reaches this state unless
    /// somebody set one (spec §A.10).
    Expired,
    /// Discarded (job discarded, session shutdown) before authorizing an
    /// execution.
    Abandoned,
    /// Preconditions failed, or 5 rejected credentials — dead, re-request
    /// required.
    Voided,
}

/// What an `Expired` entry's `what` names: which deadline was observed to
/// have passed (spec §B.1). Neither is enforced on a timer — both are
/// readings from the clock the embedder installed, compared when the
/// request is next observed (spec §A.10).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Expiring {
    /// The optional deadline an embedder set on the request. Requests
    /// carry none by default.
    Request,
    /// The grant's `not_after` — decided, but never (successfully) redeemed
    /// before it passed.
    Grant,
}

/// Why an undecided request was closed from the requesting side (spec §B.5).
/// Cancellation is what replaced expiry: the kernel does not time a request
/// out, so something must be able to end one.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "reason", rename_all = "snake_case")]
pub enum CancelReason {
    /// The requesting side stopped wanting it: job discarded, session
    /// ended, the agent moved on.
    Withdrawn,
    /// An embedder's own deadline policy closed it. The kernel records
    /// this; it never originates it (spec §A.10).
    DeadlinePassed,
    /// Superseded by a later request for the same intent.
    Superseded {
        /// The request that replaced this one.
        by: RequestId,
    },
}

impl std::fmt::Display for CancelReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Withdrawn => write!(f, "withdrawn by the requester"),
            Self::DeadlinePassed => write!(f, "the embedder's deadline passed"),
            Self::Superseded { by } => write!(f, "superseded by {by}"),
        }
    }
}

// ───────────────────────── The entry log ─────────────────────────

/// One append to the ledger. Internally tagged on the `"entry"` key so
/// NDJSON stays one self-describing line per entry (spec §A.5). `seq` is
/// monotonic per ledger, and so is `at`: the ledger latches the largest
/// reading it has taken from the clock the embedder installed, so entry
/// stamps never regress and `seq` order and `at` order can never disagree.
///
/// No entry carries a credential, so the whole log is safe to stream to a
/// sink, project into `/v/approvals`, and print.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "entry", rename_all = "snake_case")]
pub enum LedgerEntry {
    /// The implementation side posted a request.
    Requested {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The posted request. Boxed because it is by far the widest payload
        /// any entry carries — a `Plan` alone is three collections — and
        /// every other variant would otherwise pay its size on every clone
        /// into the ring and every send to the sink. `Box<T>` serializes as
        /// `T`, so the wire shape is unchanged.
        request: Box<ApprovalRequest>,
    },
    /// The approval side posted a grant.
    Granted {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The posted grant.
        grant: Grant,
    },
    /// The approval side posted a denial.
    Denied {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The denied request.
        request: RequestId,
        /// Who denied it.
        by: Principal,
        /// Why.
        reason: String,
    },
    /// A deadline passed with no closing decision — a grant's `not_after`,
    /// or the optional deadline an embedder set on the request.
    Expired {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The expired request.
        request: RequestId,
        /// Which deadline passed.
        what: Expiring,
    },
    /// The approval side retrieved the key. Appended on every retrieval, so
    /// a key that leaves the kernel has a name attached to its departure
    /// (spec §A.2).
    KeyRetrieved {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request whose key was retrieved.
        request: RequestId,
        /// Who retrieved it.
        by: Principal,
    },
    /// An attempt was reserved.
    Redeemed {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request being redeemed.
        request: RequestId,
        /// The attempt this reservation allocated.
        attempt: AttemptId,
        /// The principal that presented the key or held the redemption
        /// context — the other half of the accountability pair with
        /// `KeyRetrieved` (spec §A.2).
        by: Principal,
        /// What the condition check saw, and when.
        observed: Vec<Observation>,
    },
    /// Preconditions no longer hold. Voids the grant and reserves no
    /// attempt.
    Refused {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request whose redemption was refused.
        request: RequestId,
        /// The condition that failed.
        condition: Condition,
        /// What was actually observed.
        found: StateClaim,
    },
    /// An attempt reported a terminal outcome.
    Settled {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request this attempt belongs to.
        request: RequestId,
        /// The attempt that settled.
        attempt: AttemptId,
        /// How it ended.
        outcome: Outcome,
    },
    /// A request or an attempt was abandoned.
    Abandoned {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request abandoned.
        request: RequestId,
        /// `None` when the request was abandoned before any attempt was
        /// reserved; `Some` when an attempt was running and its executor is
        /// gone — which does NOT mean nothing happened.
        attempt: Option<AttemptId>,
        /// Why.
        reason: String,
    },
    /// A request's chain was voided (stale conditions, or 5 rejected
    /// credentials).
    Voided {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request voided.
        request: RequestId,
        /// Why.
        reason: String,
    },
    /// A standing grant was issued.
    StandingIssued {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The standing grant issued.
        grant: StandingGrant,
    },
    /// A standing grant was revoked.
    StandingRevoked {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The standing grant revoked.
        id: StandingId,
        /// Who revoked it.
        by: Principal,
        /// Why.
        reason: String,
    },
    /// A subscription was registered.
    Subscribed {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The subscription registered, carrying its allocated id.
        subscription: Subscription,
    },
    /// An `observe` subscription covered a mutation, which proceeded. A
    /// record with no chain behind it: no request was built, no grant
    /// exists, and there is nothing to redeem or settle. Posted before the
    /// mutation runs, so it records that the operation was dispatched under
    /// the subscription's scope — not that it succeeded; the command's own
    /// exit code carries that.
    Observed {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The operation observed.
        operation: OperationId,
        /// Who ran it.
        by: Principal,
        /// Every covered resource, each naming the subscription that
        /// covered it — one command's covered paths post as one entry,
        /// however many subscriptions their coverage came from. The
        /// statement tap records one `cmd` resource per planned command,
        /// covered by no subscription.
        resources: Vec<ObservedResource>,
        /// The statement plan, present exactly on a `cmd.execute` entry
        /// from the statement tap (spec §C.6). `None` for every `fs.*`
        /// entry — a path batch has no statement behind it to plan.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        plan: Option<Plan>,
    },
    /// A subscription was revoked. Takes effect immediately for operations
    /// not yet posted; requests already granted under it are unaffected.
    Unsubscribed {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The subscription revoked.
        id: SubscriptionId,
        /// Who revoked it.
        by: Principal,
        /// Why.
        reason: String,
    },
    /// An undecided request was closed from the requesting side (spec
    /// §B.5). With no expiry, this is the only way a request nobody decides
    /// ever ends.
    Cancelled {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// The request that was closed.
        request: RequestId,
        /// Who closed it.
        by: Principal,
        /// Why.
        reason: CancelReason,
    },
    /// A bad credential was presented.
    TokenRejected {
        /// Monotonic per-ledger sequence number.
        seq: u64,
        /// The clock reading this entry was committed at.
        #[serde(with = "crate::rfc3339::system_time")]
        at: SystemTime,
        /// `Some` when the presenting draft matched a live request (so the
        /// count means something); `None` when it matched nothing.
        request: Option<RequestId>,
        /// The running rejection count against `request`. The fifth
        /// rejection against one request voids it (spec §F.3).
        attempts: u32,
    },
}

impl LedgerEntry {
    /// This entry's monotonic per-ledger sequence number. Matched
    /// exhaustively *here*, inside the defining crate — a variant added
    /// without extending this match is a compile error, unlike a
    /// downstream `#[non_exhaustive]` match that would need a silent
    /// wildcard arm (spec §A.6's anti-drift template, applied to `seq`).
    pub fn seq(&self) -> u64 {
        match self {
            Self::Requested { seq, .. }
            | Self::Granted { seq, .. }
            | Self::Denied { seq, .. }
            | Self::Expired { seq, .. }
            | Self::KeyRetrieved { seq, .. }
            | Self::Redeemed { seq, .. }
            | Self::Refused { seq, .. }
            | Self::Settled { seq, .. }
            | Self::Abandoned { seq, .. }
            | Self::Voided { seq, .. }
            | Self::StandingIssued { seq, .. }
            | Self::StandingRevoked { seq, .. }
            | Self::Subscribed { seq, .. }
            | Self::Observed { seq, .. }
            | Self::Unsubscribed { seq, .. }
            | Self::Cancelled { seq, .. }
            | Self::TokenRejected { seq, .. } => *seq,
        }
    }

    /// When this entry was appended. Matched exhaustively here for the same
    /// reason [`Self::seq`] is — a variant added without extending this match
    /// is a compile error.
    pub fn at(&self) -> SystemTime {
        match self {
            Self::Requested { at, .. }
            | Self::Granted { at, .. }
            | Self::Denied { at, .. }
            | Self::Expired { at, .. }
            | Self::KeyRetrieved { at, .. }
            | Self::Redeemed { at, .. }
            | Self::Refused { at, .. }
            | Self::Settled { at, .. }
            | Self::Abandoned { at, .. }
            | Self::Voided { at, .. }
            | Self::StandingIssued { at, .. }
            | Self::StandingRevoked { at, .. }
            | Self::Subscribed { at, .. }
            | Self::Observed { at, .. }
            | Self::Unsubscribed { at, .. }
            | Self::Cancelled { at, .. }
            | Self::TokenRejected { at, .. } => *at,
        }
    }

    /// The request this entry is about, when it is about one. `None` for the
    /// entries with no single owning request: `StandingIssued`,
    /// `StandingRevoked`, `Subscribed`, `Unsubscribed`, `Observed` (a
    /// chainless record — spec §C.5), and a `TokenRejected` that matched no
    /// live request.
    pub fn request(&self) -> Option<&RequestId> {
        match self {
            Self::Requested { request, .. } => Some(&request.id),
            Self::Granted { grant, .. } => Some(&grant.request),
            Self::Denied { request, .. }
            | Self::Expired { request, .. }
            | Self::KeyRetrieved { request, .. }
            | Self::Redeemed { request, .. }
            | Self::Refused { request, .. }
            | Self::Settled { request, .. }
            | Self::Abandoned { request, .. }
            | Self::Voided { request, .. }
            | Self::Cancelled { request, .. } => Some(request),
            Self::TokenRejected { request, .. } => request.as_ref(),
            Self::StandingIssued { .. }
            | Self::StandingRevoked { .. }
            | Self::Subscribed { .. }
            | Self::Unsubscribed { .. }
            | Self::Observed { .. } => None,
        }
    }

    /// Whether appending this entry bumps its request's
    /// [`revision`](ApprovalRequest::revision) — spec §A.7's "every recorded
    /// transition bumps `revision`".
    ///
    /// Two entries that name a request are **not** transitions of it.
    /// `Requested` creates the request at revision 0; there is nothing to
    /// bump yet. `KeyRetrieved` records that a key left the kernel and moves
    /// nothing on the state machine — bumping there would invalidate the
    /// revision an approver is holding for a decision it has not made yet.
    pub fn bumps_revision(&self) -> bool {
        match self {
            Self::Requested { .. } | Self::KeyRetrieved { .. } => false,
            _ => self.request().is_some(),
        }
    }
}

// ───────────────────────── The record envelope ─────────────────────────

/// The [`LedgerRecord::schema_version`] this build writes.
///
/// It bumps when a reader **must notice** a change, not on every addition —
/// a version number nobody knows how to react to is decoration. Additions
/// that a reader can ignore (a new optional field, a new entry variant a
/// reader is required to surface as unknown anyway) do not bump it.
pub const LEDGER_SCHEMA_VERSION: u16 = 1;

/// A ledger entry as a consumer reads it: versioned, sequenced, scoped
/// (spec §A.5).
///
/// Entries are read through this wrapper, never as a bare [`LedgerEntry`], so
/// a consumer written against one release can be handed a later ledger's
/// output and know what it is holding.
///
/// **Compatibility rules**, so `schema_version` is worth carrying:
///
/// - Every public type in this module is `#[non_exhaustive]`, and every field
///   added later carries a serde default.
/// - A reader **must tolerate unknown object fields** — they are a newer
///   writer's additions, not corruption.
/// - A reader **must not silently drop an unknown entry variant or an
///   unrecognized `schema_version`.** Deserializing an unrecognized entry
///   yields [`RecordedEntry::Unknown`] with its `sequence` and `scope`
///   intact, so a gap in an audit log is visible as a gap. Dropping it would
///   let a reader report a clean history it did not actually verify.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LedgerRecord {
    /// The schema this record was written against. Compare it against
    /// [`LEDGER_SCHEMA_VERSION`] — see [`Self::schema_is_known`].
    pub schema_version: u16,
    /// The entry's monotonic per-ledger sequence number.
    pub sequence: u64,
    /// Wall-clock append time.
    #[serde(with = "crate::rfc3339::system_time")]
    pub at: SystemTime,
    /// Which kernel, session, and actor this record belongs to (spec §A.7).
    pub scope: ApprovalScope,
    /// The entry itself, or the fact that this build does not recognize it.
    pub entry: RecordedEntry,
}

impl LedgerRecord {
    /// Wrap an entry this build wrote. `sequence` and `at` are read off the
    /// entry rather than supplied, so the envelope and its payload cannot
    /// disagree about when a thing happened or in what order.
    pub fn new(scope: ApprovalScope, entry: LedgerEntry) -> Self {
        Self {
            schema_version: LEDGER_SCHEMA_VERSION,
            sequence: entry.seq(),
            at: entry.at(),
            scope,
            entry: RecordedEntry::Known(entry),
        }
    }

    /// Whether this build understands the schema this record was written
    /// against. `false` means read `sequence`, `at`, and `scope` and treat
    /// the entry as opaque — never as absent.
    pub fn schema_is_known(&self) -> bool {
        self.schema_version <= LEDGER_SCHEMA_VERSION
    }

    /// The entry, when this build recognizes it. `None` is an entry a newer
    /// writer produced — a fact to surface, not a record to drop.
    pub fn known(&self) -> Option<&LedgerEntry> {
        match &self.entry {
            RecordedEntry::Known(entry) => Some(entry),
            RecordedEntry::Unknown(_) => None,
        }
    }
}

/// An entry this build recognizes, or one it does not (spec §A.5).
///
/// Untagged on the wire: a record written by any version of kaish
/// deserializes as `Known` when this build has the variant and `Unknown` when
/// it does not. There is no third outcome — an entry never disappears.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RecordedEntry {
    /// An entry this build has a variant for.
    Known(LedgerEntry),
    /// An entry this build does not recognize, kept verbatim.
    Unknown(UnknownEntry),
}

/// An entry a newer writer produced, carried through verbatim (spec §A.5).
///
/// Round-tripping it unchanged is the point: a reader that re-exports a log
/// must not silently narrow it to the variants it happened to know about.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnknownEntry {
    /// The `entry` tag the writer used, e.g. `"assessed"`.
    pub entry: String,
    /// Every other field, verbatim.
    #[serde(flatten)]
    pub fields: BTreeMap<String, serde_json::Value>,
}

/// Test-only: a stamped, tokenless view, for exercising the control-plane
/// `.approval` field on `ExecResult`/`ToolResult`/`JobInfo` without standing
/// up a live ledger. One builder shared by every module's tests so the shape
/// under test cannot drift between them.
#[cfg(test)]
pub(crate) fn sample_view(operation: &str, paths: &[&str]) -> ApprovalRequestView {
    let draft = ApprovalRequest::builder(operation)
        .risk(RiskClass::Irreversible)
        .reason("the fs.* enforce policy is on")
        .hint(format!("{operation} --confirm=<token> {}", paths.join(" ")));
    paths
        .iter()
        .fold(draft, |b, p| b.resource(Resource::plain("path", *p)))
        .build()
        .expect("a well-formed draft")
        .stamp(
            RequestId::new(0x0badcafe, 1),
            std::time::UNIX_EPOCH,
            RequestOrigin::new(
                sample_scope(),
                sample_binding(),
                Principal::new("session", PrincipalKind::Agent),
                Capture::Exact(Invocation {
                    tool: operation.split('.').next().unwrap_or(operation).to_string(),
                    argv: paths.iter().map(|p| (*p).to_string()).collect(),
                }),
            ),
        )
        .into()
}

/// Test-only: a fixed scope, so a fixture's shape is stable across runs.
#[cfg(test)]
pub(crate) fn sample_scope() -> ApprovalScope {
    ApprovalScope::kernel(KernelId::new(1)).with_session(SessionId::new("session-1"))
}

/// Test-only: a binding matching [`sample_scope`].
#[cfg(test)]
pub(crate) fn sample_binding() -> PlanBinding {
    PlanBinding::new(PlanDigest::new("d1ge57"), "/w", sample_scope())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    // ── RequestId ──

    #[test]
    fn request_id_renders_in_full_form_with_no_hyphen() {
        let id = RequestId::new(0x9c1a4f2e, 42);
        assert_eq!(id.as_str(), "req_9c1a4f2e_42");
        assert!(!id.as_str().contains('-'), "must contain no hyphen: {id}");
        assert_eq!(id.to_string(), "req_9c1a4f2e_42");
    }

    #[test]
    fn request_id_round_trips_through_parse() {
        let id = RequestId::new(0x00000001, 0);
        let parsed = RequestId::parse(id.as_str()).expect("full-form id parses");
        assert_eq!(parsed, id);
    }

    #[test]
    fn request_id_short_forms_are_rejected() {
        // Epoch alone, no seq.
        assert!(RequestId::parse("req_9c1a4f2e").is_err());
        // No prefix at all.
        assert!(RequestId::parse("9c1a4f2e_42").is_err());
        // Truncated epoch.
        assert!(RequestId::parse("req_9c1a_42").is_err());
        // Trailing underscore, empty seq.
        assert!(RequestId::parse("req_9c1a4f2e_").is_err());
        // Uppercase hex is not accepted — the format is lowercase only.
        assert!(RequestId::parse("req_9C1A4F2E_42").is_err());
        // A hyphen anywhere is rejected outright — the format has none.
        assert!(RequestId::parse("req-9c1a4f2e-42").is_err());
        // Non-decimal seq.
        assert!(RequestId::parse("req_9c1a4f2e_4x").is_err());
    }

    #[test]
    fn request_id_serde_round_trips_and_rejects_short_forms() {
        let id = RequestId::new(0xdeadbeef, 7);
        let json = serde_json::to_string(&id).unwrap();
        assert_eq!(json, "\"req_deadbeef_7\"");
        let back: RequestId = serde_json::from_str(&json).unwrap();
        assert_eq!(back, id);

        // A short form fails to deserialize, not just to parse().
        let bad: Result<RequestId, _> = serde_json::from_str("\"req_deadbeef\"");
        assert!(bad.is_err());
    }

    // ── Token ──

    #[test]
    fn token_debug_never_prints_the_raw_credential() {
        let raw = "a1b2c3d4e5f60718293a4b5c6d7e8f90";
        let token = Token::new(raw);
        let debug = format!("{token:?}");
        assert!(!debug.contains(raw), "Debug leaked the credential: {debug}");
        assert!(debug.contains("redacted"), "Debug should say redacted: {debug}");
        // The prefix IS allowed to appear — it's the correlation surface.
        assert!(debug.contains(&token.token_prefix()));
    }

    #[test]
    fn token_prefix_is_first_four_chars() {
        let token = Token::new("a1b2c3d4e5f6");
        assert_eq!(token.token_prefix(), "a1b2");
    }

    #[test]
    fn token_reveal_returns_the_raw_value() {
        let token = Token::new("deadbeef");
        assert_eq!(token.reveal(), "deadbeef");
    }

    // ── OperationId ──

    #[test]
    fn operation_id_namespaced_rejects_reserved_prefixes() {
        assert!(matches!(
            OperationId::namespaced("fs", "remove"),
            Err(OperationIdError::ReservedPrefix(p)) if p == "fs"
        ));
        assert!(matches!(
            OperationId::namespaced("trash", "empty"),
            Err(OperationIdError::ReservedPrefix(p)) if p == "trash"
        ));
        // The statement gate's namespace (spec §C.6). A plugin posting
        // `cmd.execute` would pose as the kernel's own statement tap.
        assert!(matches!(
            OperationId::namespaced("cmd", "execute"),
            Err(OperationIdError::ReservedPrefix(p)) if p == "cmd"
        ));
    }

    #[test]
    fn operation_id_namespaced_allows_a_plugin_prefix() {
        let id = OperationId::namespaced("git", "push").unwrap();
        assert_eq!(id.as_str(), "git.push");
    }

    #[test]
    fn operation_id_namespaced_rejects_empty_parts() {
        assert!(OperationId::namespaced("", "push").is_err());
        assert!(OperationId::namespaced("git", "").is_err());
    }

    #[test]
    fn operation_id_new_rejects_empty() {
        assert!(matches!(OperationId::new(""), Err(OperationIdError::Empty)));
        assert!(OperationId::new("git.push").is_ok());
    }

    // ── StateClaim::Unspecified ──

    #[test]
    fn unspecified_never_equals_a_concrete_claim() {
        assert_ne!(StateClaim::Unspecified, StateClaim::Absent);
        assert_ne!(StateClaim::Unspecified, StateClaim::Exact("a1b2".to_string()));
        assert_ne!(
            StateClaim::Unspecified,
            StateClaim::Digest {
                alg: "sha256".to_string(),
                hex: "ff".to_string()
            }
        );
        // Only equals itself.
        assert_eq!(StateClaim::Unspecified, StateClaim::Unspecified);
    }

    // ── ApprovalRequest builder ──

    #[test]
    fn empty_operation_fails_to_build() {
        let err = ApprovalRequest::builder("")
            .risk(RiskClass::Reversible)
            .build()
            .unwrap_err();
        assert_eq!(err, ApprovalRequestBuildError::EmptyOperation);
    }

    #[test]
    fn missing_risk_fails_to_build() {
        let err = ApprovalRequest::builder("git.push").build().unwrap_err();
        assert_eq!(err, ApprovalRequestBuildError::MissingRisk);
    }

    #[test]
    fn builder_drafts_carry_no_principal_or_capture() {
        // Structural: ApprovalRequestDraft has no `principal` and no
        // `capture` field at all — this destructure is exhaustive (no `..`)
        // and would fail to compile if either field existed.
        let draft = ApprovalRequest::builder("git.push")
            .risk(RiskClass::Irreversible)
            .resource(Resource::plain("git.remote", "origin"))
            .reason("pushing to a protected branch")
            .hint("git push --confirm=<token> origin main")
            .build()
            .expect("valid draft");
        let ApprovalRequestDraft {
            operation,
            risk,
            resources,
            reason,
            hint,
            supersedes,
            plan,
        } = draft;
        assert_eq!(operation.as_str(), "git.push");
        assert_eq!(risk, RiskClass::Irreversible);
        assert_eq!(resources.len(), 1);
        assert_eq!(reason, "pushing to a protected branch");
        assert!(hint.contains("<token>"));
        assert!(supersedes.is_none());
        // Only the statement gate attaches a plan (spec §C.6).
        assert!(plan.is_none());
    }

    #[test]
    fn stamp_turns_a_draft_into_a_full_request() {
        let draft = ApprovalRequest::builder("git.push")
            .risk(RiskClass::Irreversible)
            .build()
            .unwrap();
        let req = draft.stamp(
            RequestId::new(1, 1),
            SystemTime::UNIX_EPOCH,
            RequestOrigin::new(
                sample_scope(),
                sample_binding(),
                Principal::new("agent-1", PrincipalKind::Agent),
                Capture::DirectExecution,
            ),
        );
        assert_eq!(req.id.as_str(), "req_00000001_1");
        assert_eq!(req.principal.id, "agent-1");
        assert_eq!(req.capture, Capture::DirectExecution);
    }

    // ── GrantTerms ──

    #[test]
    fn once_for_copies_declared_transitions_into_conditions() {
        let draft = ApprovalRequest::builder("git.push")
            .risk(RiskClass::Irreversible)
            .resource(Resource::transition(
                "git.ref",
                "refs/heads/main",
                StateClaim::Exact("a1b2".to_string()),
                StateClaim::Exact("c3d4".to_string()),
            ))
            .resource(Resource::plain("git.remote", "origin"))
            .build()
            .unwrap();
        let req = draft.stamp(
            RequestId::new(1, 1),
            SystemTime::UNIX_EPOCH,
            RequestOrigin::new(
                sample_scope(),
                sample_binding(),
                Principal::default(),
                Capture::DirectExecution,
            ),
        );
        let not_after = SystemTime::UNIX_EPOCH + Duration::from_secs(300);
        let terms = GrantTerms::once_for(&req, not_after);
        // Only the resource that declared a transition produces a condition.
        assert_eq!(terms.conditions.len(), 1);
        assert_eq!(terms.conditions[0].resource.kind, "git.ref");
        assert_eq!(terms.conditions[0].expected_from, StateClaim::Exact("a1b2".to_string()));
        assert_eq!(terms.not_after, not_after);
    }

    // ── Structural API-surface proofs: no Token field anywhere ──
    //
    // Token deliberately has no Serialize/Deserialize impl (see its doc
    // comment): that is the durable guarantee, because it means a struct in
    // this module that grew a `token: Token` field and derived Serialize
    // (every wide record here does) would fail to compile, not just fail a
    // test. The exhaustive destructures below are a second, narrower proof
    // for the specific structs the spec calls out — each list is closed (no
    // `..`), so an added field forces this test to be updated, and the type
    // ascriptions on the security-relevant fields catch a `Token` swap
    // directly.

    #[test]
    fn approval_request_has_no_credential_field() {
        let draft = ApprovalRequest::builder("git.push")
            .risk(RiskClass::Reversible)
            .build()
            .unwrap();
        let req = draft.stamp(
            RequestId::new(1, 1),
            SystemTime::UNIX_EPOCH,
            RequestOrigin::new(
                sample_scope(),
                sample_binding(),
                Principal::default(),
                Capture::DirectExecution,
            ),
        );
        let ApprovalRequest {
            id,
            scope,
            parent,
            revision,
            binding,
            operation,
            risk,
            resources,
            principal,
            capture,
            context,
            job_id,
            reason,
            hint,
            requested_at,
            deadline,
            supersedes,
            plan,
        } = req;
        let _: Option<Plan> = plan;
        let _: RequestId = id;
        let _: ApprovalScope = scope;
        let _: Option<RequestId> = parent;
        let _: u64 = revision;
        let _: PlanBinding = binding;
        let _: OperationId = operation;
        let _: RiskClass = risk;
        let _: Vec<Resource> = resources;
        let _: Principal = principal;
        let _: Capture = capture;
        let _: RequestContext = context;
        let _: Option<u64> = job_id;
        let _: String = reason;
        let _: String = hint;
        let _: SystemTime = requested_at;
        let _: Option<SystemTime> = deadline;
        let _: Option<RequestId> = supersedes;
    }

    #[test]
    fn approval_request_view_has_no_credential_field() {
        let draft = ApprovalRequest::builder("git.push")
            .risk(RiskClass::Reversible)
            .build()
            .unwrap();
        let req = draft.stamp(
            RequestId::new(1, 1),
            SystemTime::UNIX_EPOCH,
            RequestOrigin::new(
                sample_scope(),
                sample_binding(),
                Principal::default(),
                Capture::DirectExecution,
            ),
        );
        let view: ApprovalRequestView = req.into();
        let ApprovalRequestView {
            id,
            scope,
            parent,
            revision,
            binding,
            operation,
            risk,
            resources,
            principal,
            capture,
            context,
            job_id,
            reason,
            hint,
            requested_at,
            deadline,
            supersedes,
            plan,
        } = view;
        let _: Option<Plan> = plan;
        let _: RequestId = id;
        let _: ApprovalScope = scope;
        let _: Option<RequestId> = parent;
        let _: u64 = revision;
        let _: PlanBinding = binding;
        let _: OperationId = operation;
        let _: RiskClass = risk;
        let _: Vec<Resource> = resources;
        let _: Principal = principal;
        let _: Capture = capture;
        let _: RequestContext = context;
        let _: Option<u64> = job_id;
        let _: String = reason;
        let _: String = hint;
        let _: SystemTime = requested_at;
        let _: Option<SystemTime> = deadline;
        let _: Option<RequestId> = supersedes;
    }

    #[test]
    fn grant_has_no_redemption_limit_field() {
        let grant = Grant {
            request: RequestId::new(1, 1),
            decided_by: Principal::default(),
            grounds: Grounds::Embedder,
            not_after: SystemTime::UNIX_EPOCH,
            token_prefix: "a1b2".to_string(),
            conditions: Vec::new(),
            decided_at: SystemTime::UNIX_EPOCH,
        };
        // Exhaustive destructure: the single-success rule is structural —
        // there is no field here to configure a redemption count.
        let Grant {
            request,
            decided_by,
            grounds,
            not_after,
            token_prefix,
            conditions,
            decided_at,
        } = grant;
        let _: RequestId = request;
        let _: Principal = decided_by;
        let _: Grounds = grounds;
        let _: SystemTime = not_after;
        let _: String = token_prefix;
        let _: Vec<Condition> = conditions;
        let _: SystemTime = decided_at;
    }

    // ── serde round-trip: every LedgerEntry variant, including the tag ──

    fn sample_request() -> ApprovalRequest {
        ApprovalRequest::builder("git.push")
            .risk(RiskClass::Irreversible)
            .resource(Resource::transition(
                "git.ref",
                "refs/heads/main",
                StateClaim::Exact("a1b2".to_string()),
                StateClaim::Exact("c3d4".to_string()),
            ))
            .reason("pushing to a protected branch")
            .hint("git push --confirm=<token> origin main")
            .build()
            .unwrap()
            .stamp(
                RequestId::new(1, 1),
                SystemTime::UNIX_EPOCH,
                RequestOrigin::new(
                    sample_scope(),
                    sample_binding(),
                    Principal::new("agent-1", PrincipalKind::Agent),
                    Capture::Exact(Invocation {
                        tool: "git".to_string(),
                        argv: vec!["push".to_string(), "origin".to_string(), "main".to_string()],
                    }),
                ),
            )
    }

    fn sample_grant() -> Grant {
        Grant {
            request: RequestId::new(1, 1),
            decided_by: Principal::new("amy", PrincipalKind::Human),
            grounds: Grounds::Human {
                channel: "repl".to_string(),
            },
            not_after: SystemTime::UNIX_EPOCH + Duration::from_secs(300),
            token_prefix: "a1b2".to_string(),
            conditions: vec![Condition {
                resource: ResourceRef {
                    kind: "git.ref".to_string(),
                    id: "refs/heads/main".to_string(),
                },
                expected_from: StateClaim::Exact("a1b2".to_string()),
            }],
            decided_at: SystemTime::UNIX_EPOCH,
        }
    }

    fn sample_standing_grant() -> StandingGrant {
        StandingGrant {
            id: StandingId::new(1),
            operations: vec![OperationPattern::new("git.commit")],
            resources: vec![ResourcePattern::new("git.ref", "refs/heads/agent/*")],
            principal: None,
            max_uses: Some(10),
            expires_at: None,
            issued_by: Principal::new("amy", PrincipalKind::Human),
            reason: "trust agent branches".to_string(),
        }
    }

    #[test]
    fn standing_grant_missing_max_uses_on_the_wire_is_one_shot_not_unlimited() {
        let mut value = serde_json::to_value(sample_standing_grant()).unwrap();
        value.as_object_mut().unwrap().remove("max_uses");
        let parsed: StandingGrant = serde_json::from_value(value).unwrap();
        assert_eq!(parsed.max_uses, Some(1));
    }

    #[test]
    fn standing_grant_explicit_null_max_uses_is_unlimited_not_the_default() {
        // The null-versus-omitted split is deliberate and this test pins it:
        // null is the wire spelling of an explicit unlimited, omission is
        // the one-shot default. A producer meaning "default" must omit.
        let mut value = serde_json::to_value(sample_standing_grant()).unwrap();
        value
            .as_object_mut()
            .unwrap()
            .insert("max_uses".to_string(), serde_json::Value::Null);
        let parsed: StandingGrant = serde_json::from_value(value).unwrap();
        assert_eq!(parsed.max_uses, None);
    }

    #[test]
    fn standing_grant_is_one_shot_by_default_and_widening_is_explicit() {
        let base = || {
            StandingGrant::new(
                vec![OperationPattern::new("git.commit")],
                Vec::new(),
                None,
                None,
                Principal::new("amy", PrincipalKind::Human),
                "one-shot unless widened",
            )
        };
        assert_eq!(base().max_uses, Some(1));
        assert_eq!(base().with_max_uses(5).max_uses, Some(5));
        assert_eq!(base().unlimited_uses().max_uses, None);
    }

    fn all_entries() -> Vec<LedgerEntry> {
        let at = SystemTime::UNIX_EPOCH;
        let request = RequestId::new(1, 1);
        let by = Principal::new("amy", PrincipalKind::Human);
        vec![
            LedgerEntry::Requested {
                seq: 1,
                at,
                request: Box::new(sample_request()),
            },
            LedgerEntry::Granted {
                seq: 2,
                at,
                grant: sample_grant(),
            },
            LedgerEntry::Denied {
                seq: 3,
                at,
                request: request.clone(),
                by: by.clone(),
                reason: "no".to_string(),
            },
            LedgerEntry::Expired {
                seq: 4,
                at,
                request: request.clone(),
                what: Expiring::Request,
            },
            LedgerEntry::KeyRetrieved {
                seq: 5,
                at,
                request: request.clone(),
                by: by.clone(),
            },
            LedgerEntry::Redeemed {
                seq: 6,
                at,
                request: request.clone(),
                attempt: AttemptId::new(1),
                by: by.clone(),
                observed: vec![Observation {
                    resource: ResourceRef {
                        kind: "git.ref".to_string(),
                        id: "refs/heads/main".to_string(),
                    },
                    claim: StateClaim::Exact("a1b2".to_string()),
                    at,
                }],
            },
            LedgerEntry::Refused {
                seq: 7,
                at,
                request: request.clone(),
                condition: Condition {
                    resource: ResourceRef {
                        kind: "git.ref".to_string(),
                        id: "refs/heads/main".to_string(),
                    },
                    expected_from: StateClaim::Exact("a1b2".to_string()),
                },
                found: StateClaim::Exact("e5f6".to_string()),
            },
            LedgerEntry::Settled {
                seq: 8,
                at,
                request: request.clone(),
                attempt: AttemptId::new(1),
                outcome: Outcome::Exit(0),
            },
            LedgerEntry::Abandoned {
                seq: 9,
                at,
                request: request.clone(),
                attempt: Some(AttemptId::new(2)),
                reason: "process exited mid-attempt".to_string(),
            },
            LedgerEntry::Voided {
                seq: 10,
                at,
                request: request.clone(),
                reason: "5 rejected credentials".to_string(),
            },
            LedgerEntry::StandingIssued {
                seq: 11,
                at,
                grant: sample_standing_grant(),
            },
            LedgerEntry::StandingRevoked {
                seq: 12,
                at,
                id: StandingId::new(1),
                by: by.clone(),
                reason: "policy changed".to_string(),
            },
            LedgerEntry::Subscribed {
                seq: 13,
                at,
                subscription: Subscription::new(
                    vec![OperationPattern::new("fs.*")],
                    vec![ResourcePattern::new("path", "/workspace/**")],
                    SubscriptionMode::Observe,
                    "watch the workspace",
                ),
            },
            LedgerEntry::Observed {
                seq: 14,
                at,
                operation: OperationId::new("cmd.execute").expect("a valid dotted id"),
                by: by.clone(),
                resources: vec![ObservedResource::planned("cmd", "cargo")],
                plan: Some(sample_plan()),
            },
            LedgerEntry::Unsubscribed {
                seq: 15,
                at,
                id: SubscriptionId::new(1),
                by: by.clone(),
                reason: "scope narrowed".to_string(),
            },
            LedgerEntry::TokenRejected {
                seq: 16,
                at,
                request: Some(request.clone()),
                attempts: 3,
            },
        ]
    }

    /// A plan the statement tap would build for `cargo build > ${LOG}`.
    fn sample_plan() -> Plan {
        Plan::new(
            "cargo build > ${LOG}",
            "command",
            vec![PlannedCommand::new(
                "cargo",
                vec!["build".to_string()],
                vec![PlannedRedirect::new(">", "${LOG}")],
                false,
            )],
        )
    }

    #[test]
    fn request_level_abandoned_and_bogus_token_round_trip() {
        // The two Option-None shapes the main fixture doesn't cover: a request
        // abandoned before any attempt was reserved, and a bad key that
        // matched no live request at all.
        let at = SystemTime::UNIX_EPOCH;
        for entry in [
            LedgerEntry::Abandoned {
                seq: 1,
                at,
                request: RequestId::new(0x9c1a4f2e, 7),
                attempt: None,
                reason: "session shutdown before decision".to_string(),
            },
            LedgerEntry::TokenRejected {
                seq: 2,
                at,
                request: None,
                attempts: 1,
            },
        ] {
            let json = serde_json::to_value(&entry).expect("serialize");
            let back: LedgerEntry = serde_json::from_value(json).expect("deserialize");
            assert_eq!(entry, back);
        }
    }

    #[test]
    fn ledger_entry_timestamps_serialize_as_rfc3339_utc_strings() {
        // Same wire convention JobInfo pinned in kaish PR #273: every
        // SystemTime on the serde surface is an RFC 3339 UTC string.
        let entry = LedgerEntry::KeyRetrieved {
            seq: 1,
            at: SystemTime::UNIX_EPOCH,
            request: RequestId::new(0x9c1a4f2e, 7),
            by: Principal::new("amy", PrincipalKind::Human),
        };
        let json = serde_json::to_value(&entry).expect("serialize");
        assert_eq!(
            json.get("at").and_then(|v| v.as_str()),
            Some("1970-01-01T00:00:00.000Z"),
            "at must be an RFC 3339 string, got: {json}"
        );
    }

    #[test]
    fn request_id_parse_canonicalizes_leading_zero_seq() {
        let id = RequestId::parse("req_9c1a4f2e_042").expect("valid full form");
        assert_eq!(id.as_str(), "req_9c1a4f2e_42");
        assert_eq!(id, RequestId::parse("req_9c1a4f2e_42").expect("canonical"));
    }

    #[test]
    fn decision_wire_spellings_are_snake_case() {
        let deny = Decision::Deny {
            reason: "nope".to_string(),
        };
        let json = serde_json::to_value(&deny).expect("serialize");
        assert!(json.get("deny").is_some(), "expected snake_case tag: {json}");
        let defer = serde_json::to_value(Decision::Defer).expect("serialize");
        assert_eq!(defer, serde_json::json!("defer"));
    }

    const EXPECTED_TAGS: &[&str] = &[
        "requested",
        "granted",
        "denied",
        "expired",
        "key_retrieved",
        "redeemed",
        "refused",
        "settled",
        "abandoned",
        "voided",
        "standing_issued",
        "standing_revoked",
        "subscribed",
        "observed",
        "unsubscribed",
        "token_rejected",
    ];

    #[test]
    fn every_ledger_entry_variant_round_trips_with_its_tag() {
        let entries = all_entries();
        assert_eq!(
            entries.len(),
            EXPECTED_TAGS.len(),
            "every LedgerEntry variant must have a sample here"
        );
        for (entry, expected_tag) in entries.into_iter().zip(EXPECTED_TAGS) {
            let json = serde_json::to_value(&entry).unwrap();
            assert_eq!(
                json.get("entry").and_then(|v| v.as_str()),
                Some(*expected_tag),
                "wrong tag for {entry:?}: {json}"
            );
            let back: LedgerEntry = serde_json::from_value(json.clone()).unwrap();
            assert_eq!(back, entry, "round-trip mismatch: {json}");
        }
    }

    #[test]
    fn ledger_entry_by_field_present_on_redeemed_and_key_retrieved() {
        let entries = all_entries();
        for entry in &entries {
            let json = serde_json::to_value(entry).unwrap();
            match entry {
                LedgerEntry::KeyRetrieved { .. } | LedgerEntry::Redeemed { .. } => {
                    assert!(json.get("by").is_some(), "{json} must carry `by`");
                }
                _ => {}
            }
        }
    }

    // ── The statement plan (spec §C.6) ──

    #[test]
    fn capture_statement_round_trips_with_its_source_and_index() {
        // The capture `Kernel::confirm` replays from: statements carry no
        // source spans, so the whole program plus an index is the capture.
        let capture = Capture::Statement {
            source: "x=1\nrm -rf ${DIR}".to_string(),
            index: 1,
        };
        let json = serde_json::to_value(&capture).expect("serialize");
        let back: Capture = serde_json::from_value(json).expect("deserialize");
        assert_eq!(capture, back);
    }

    #[test]
    fn plan_round_trips_with_every_planned_field() {
        let plan = sample_plan();
        let json = serde_json::to_value(&plan).expect("serialize");
        let back: Plan = serde_json::from_value(json).expect("deserialize");
        assert_eq!(plan, back);
        assert_eq!(back.commands[0].redirects[0].kind, ">");
        // Unexpanded: the target keeps `${LOG}` as written, because a
        // classifier judges what was asked, not what it resolved to.
        assert_eq!(back.commands[0].redirects[0].target, "${LOG}");
    }

    #[test]
    fn a_request_without_a_plan_omits_the_field_on_the_wire() {
        // `plan` is additive: an `fs.*` request serializes exactly as it did
        // before the statement gate existed.
        let req = sample_request();
        assert!(req.plan.is_none());
        let json = serde_json::to_value(&req).expect("serialize");
        assert!(json.get("plan").is_none(), "plan must be omitted: {json}");
        let back: ApprovalRequest = serde_json::from_value(json).expect("deserialize");
        assert_eq!(req, back);
    }

    #[test]
    fn a_planned_resource_names_no_subscription() {
        // `cmd.*` never enters the subscription registry (spec §C.6), so the
        // statement tap's resources carry no subscription id — and the field
        // is omitted rather than faked with a sentinel.
        let resource = ObservedResource::planned("cmd", "rm");
        assert_eq!(resource.subscription, None);
        assert_eq!(resource.resolved, "rm");
        let json = serde_json::to_value(&resource).expect("serialize");
        assert!(
            json.get("subscription").is_none(),
            "an unsubscribed resource must omit the field: {json}"
        );
    }

    #[test]
    fn a_subscribed_resource_still_names_its_subscription() {
        let resource = ObservedResource::new("path", "x.txt", "/w/x.txt", SubscriptionId::new(7));
        assert_eq!(resource.subscription, Some(SubscriptionId::new(7)));
        let json = serde_json::to_value(&resource).expect("serialize");
        assert_eq!(json.get("subscription"), Some(&serde_json::json!(7)));
    }

    // ── Outcome: no plain Cancelled variant ──

    #[test]
    fn outcome_unknown_carries_a_lost_cause_not_a_bare_cancelled() {
        let outcome = Outcome::Unknown {
            cause: LostCause::Cancelled,
        };
        let json = serde_json::to_value(&outcome).unwrap();
        assert_eq!(json["unknown"]["cause"], "cancelled");
        // Confirms there is no bare "cancelled" outcome sitting next to
        // Exit/Error — every lost-executor outcome carries a cause.
        assert!(json.get("cancelled").is_none());
        let back: Outcome = serde_json::from_value(json).unwrap();
        assert_eq!(back, outcome);
    }

    // ── Principal / PrincipalKind defaults ──

    #[test]
    fn principal_defaults_to_unknown_kind() {
        assert_eq!(Principal::default().kind, PrincipalKind::Unknown);
    }

    // ── Scope (spec §A.7) ──

    #[test]
    fn minted_kernel_ids_are_distinct() {
        assert_ne!(KernelId::mint(), KernelId::mint());
    }

    #[test]
    fn a_scoped_handle_sees_only_its_own_session() {
        let kernel = KernelId::new(1);
        let mine = ApprovalScope::kernel(kernel).with_session(SessionId::new("a"));
        let theirs = ApprovalScope::kernel(kernel).with_session(SessionId::new("b"));
        let unattributed = ApprovalScope::kernel(kernel);
        let a = SessionId::new("a");
        assert!(mine.in_session(&a));
        assert!(!theirs.in_session(&a));
        // An unattributed request belongs to the kernel, not to whichever
        // session asks first.
        assert!(!unattributed.in_session(&a));
    }

    #[test]
    fn a_single_session_scope_omits_the_optional_ids_on_the_wire() {
        let json = serde_json::to_value(ApprovalScope::kernel(KernelId::new(7))).unwrap();
        assert_eq!(json, serde_json::json!({ "kernel_id": 7 }));
        let back: ApprovalScope = serde_json::from_value(json).unwrap();
        assert_eq!(back, ApprovalScope::kernel(KernelId::new(7)));
    }

    // ── Binding (spec §A.9) ──

    #[test]
    fn a_binding_matches_itself_and_names_the_first_difference_otherwise() {
        let approved = sample_binding();
        assert_eq!(approved.mismatch(&approved), None);

        let moved = PlanBinding::new(PlanDigest::new("d1ge57"), "/elsewhere", sample_scope());
        let detail = moved.mismatch(&approved).expect("a cwd change is a mismatch");
        assert!(detail.contains("/elsewhere"), "{detail}");
        assert!(detail.contains("/w"), "{detail}");

        let other_session = PlanBinding::new(
            PlanDigest::new("d1ge57"),
            "/w",
            ApprovalScope::kernel(KernelId::new(1)).with_session(SessionId::new("session-2")),
        );
        assert!(other_session
            .mismatch(&approved)
            .expect("a scope change is a mismatch")
            .contains("session-2"));

        let other_plan = PlanBinding::new(PlanDigest::new("0ther"), "/w", sample_scope());
        assert!(other_plan
            .mismatch(&approved)
            .expect("a digest change is a mismatch")
            .contains("0ther"));

        let sandboxed = sample_binding().with_sandbox_profile(SandboxProfileId::new("readonly"));
        assert!(sandboxed
            .mismatch(&approved)
            .expect("a profile change is a mismatch")
            .contains("readonly"));
    }

    // ── The record envelope (spec §A.5) ──

    #[test]
    fn a_record_derives_its_sequence_and_time_from_the_entry_it_wraps() {
        // The envelope and its payload cannot disagree about when a thing
        // happened or in what order, because the envelope does not get to
        // say.
        let entry = LedgerEntry::Voided {
            seq: 42,
            at: SystemTime::UNIX_EPOCH + Duration::from_secs(9),
            request: RequestId::new(1, 1),
            reason: "5 rejected credentials".to_string(),
        };
        let record = LedgerRecord::new(sample_scope(), entry.clone());
        assert_eq!(record.sequence, 42);
        assert_eq!(record.at, SystemTime::UNIX_EPOCH + Duration::from_secs(9));
        assert_eq!(record.schema_version, LEDGER_SCHEMA_VERSION);
        assert!(record.schema_is_known());
        assert_eq!(record.known(), Some(&entry));
    }

    #[test]
    fn every_entry_variant_round_trips_inside_a_record() {
        for entry in all_entries() {
            let record = LedgerRecord::new(sample_scope(), entry.clone());
            let json = serde_json::to_value(&record).expect("serialize");
            let back: LedgerRecord = serde_json::from_value(json.clone()).expect("deserialize");
            assert_eq!(back, record, "record round-trip mismatch: {json}");
            assert_eq!(back.known(), Some(&entry));
        }
    }

    #[test]
    fn an_unknown_entry_variant_round_trips_as_unknown_rather_than_being_dropped() {
        // A newer writer's entry: `assessed` (spec §C.7) is not a variant this
        // build has. It must survive with its sequence and scope intact — a
        // gap in an audit log has to be visible as a gap, and a reader that
        // dropped it would report a clean history it never verified.
        let json = serde_json::json!({
            "schema_version": LEDGER_SCHEMA_VERSION,
            "sequence": 17,
            "at": "1970-01-01T00:00:00.000Z",
            "scope": { "kernel_id": 1, "session_id": "session-1" },
            "entry": {
                "entry": "assessed",
                "seq": 17,
                "at": "1970-01-01T00:00:00.000Z",
                "request": "req_00000001_1",
                "assessment": { "by": "classifier", "verdict": "gate" }
            }
        });
        let record: LedgerRecord = serde_json::from_value(json.clone()).expect("deserialize");
        assert_eq!(record.sequence, 17);
        assert_eq!(record.scope.session_id, Some(SessionId::new("session-1")));
        assert_eq!(record.known(), None, "this build must not claim to know `assessed`");
        let RecordedEntry::Unknown(unknown) = &record.entry else {
            panic!("expected an unknown entry, got {:?}", record.entry);
        };
        assert_eq!(unknown.entry, "assessed");
        assert_eq!(unknown.fields["request"], serde_json::json!("req_00000001_1"));
        // Re-exporting must not narrow the log to the variants this build
        // happens to know about.
        assert_eq!(serde_json::to_value(&record).expect("serialize"), json);
    }

    #[test]
    fn an_unrecognized_schema_version_keeps_its_sequence_and_scope() {
        let mut json = serde_json::to_value(LedgerRecord::new(
            sample_scope(),
            LedgerEntry::Voided {
                seq: 3,
                at: SystemTime::UNIX_EPOCH,
                request: RequestId::new(1, 1),
                reason: "voided".to_string(),
            },
        ))
        .expect("serialize");
        json["schema_version"] = serde_json::json!(LEDGER_SCHEMA_VERSION as u32 + 999);
        let record: LedgerRecord = serde_json::from_value(json).expect("deserialize");
        assert!(!record.schema_is_known());
        assert_eq!(record.sequence, 3);
        assert_eq!(record.scope, sample_scope());
    }

    #[test]
    fn a_reader_tolerates_an_unknown_object_field_on_a_known_entry() {
        // A newer writer's added field is not corruption.
        let mut json = serde_json::to_value(LedgerRecord::new(
            sample_scope(),
            LedgerEntry::Voided {
                seq: 4,
                at: SystemTime::UNIX_EPOCH,
                request: RequestId::new(1, 1),
                reason: "voided".to_string(),
            },
        ))
        .expect("serialize");
        json["entry"]["decided_under"] = serde_json::json!("some-future-field");
        let record: LedgerRecord = serde_json::from_value(json).expect("deserialize");
        assert!(record.known().is_some(), "an added field must not force `unknown`");
    }

    // ── Revision bookkeeping (spec §A.7) ──

    #[test]
    fn a_freshly_stamped_request_starts_at_revision_zero() {
        let req = sample_request();
        assert_eq!(req.revision, 0);
        assert_eq!(req.parent, None);
    }

    #[test]
    fn every_entry_naming_a_request_bumps_revision_except_requested_and_key_retrieved() {
        for entry in all_entries() {
            let names_a_request = entry.request().is_some();
            let expected = names_a_request
                && !matches!(
                    entry,
                    LedgerEntry::Requested { .. } | LedgerEntry::KeyRetrieved { .. }
                );
            assert_eq!(
                entry.bumps_revision(),
                expected,
                "wrong revision-bump rule for {entry:?}"
            );
        }
    }

    #[test]
    fn entries_with_no_owning_request_name_none() {
        for entry in all_entries() {
            match &entry {
                LedgerEntry::StandingIssued { .. }
                | LedgerEntry::StandingRevoked { .. }
                | LedgerEntry::Subscribed { .. }
                | LedgerEntry::Unsubscribed { .. }
                | LedgerEntry::Observed { .. } => {
                    assert_eq!(entry.request(), None, "{entry:?} owns no request");
                    assert!(!entry.bumps_revision());
                }
                _ => assert!(entry.request().is_some(), "{entry:?} must name its request"),
            }
        }
    }

    #[test]
    fn every_entry_reports_the_timestamp_it_carries() {
        for entry in all_entries() {
            assert_eq!(entry.at(), SystemTime::UNIX_EPOCH, "{entry:?}");
        }
    }
}
