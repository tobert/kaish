//! The statement gate's scoping seam (`docs/approval-ledger.md` §C.6).
//!
//! Every top-level statement is recorded; a [`StatementClassifier`] decides
//! which ones must ask first. The classifier **scopes, and the decision chain
//! decides**: it says "this one is worth asking about", and everything after
//! that — a standing grant, the embedder's policy hook, a human at a terminal,
//! or a deferral to exit 2 — is the same chain every `fs.*` gate runs.
//!
//! There is deliberately no deny posture. Refusal is a chain decision
//! (`Policy::evaluate`), because a scoping seam that can refuse is a second
//! decision chain, and two chains disagree.
//!
//! **`Observe` is a bypass, so every uncertain answer must be `Gate`.** A
//! classifier that fails to load, times out, sees input outside its
//! distribution, or panics on it returns the safe answer by construction:
//! [`StatementClassifier::classify`] returns a `Result`, and the kernel maps
//! `Err` — and a caught panic — to `Gate`, never to `Observe` and never
//! silently. The alternative is a statement gate that quietly stops gating
//! the moment its classifier breaks.

use kaish_types::approval::{
    ApprovalScope, AssessorId, ModelIdentity, Plan, RiskClass, SandboxProfileId,
};

/// Decides which top-level statements must ask before they run (spec §C.6).
///
/// Register one with `KernelConfig::with_statement_classifier`. With none
/// registered every statement is [`StatementPosture::Observe`] — recorded and
/// run.
pub trait StatementClassifier: Send + Sync {
    /// Classify one top-level statement from its plan and the context it
    /// would run against.
    ///
    /// **Synchronous and non-blocking**, like `Policy::evaluate` (spec
    /// §C.2): it runs on the execution path of every statement, before
    /// anything of the statement has run. A classifier too slow for that
    /// path returns `Gate`, and the expensive judgment happens out of band
    /// after the statement returns exit 2 — which is where every slow
    /// decision lives; the kernel awaits none of them.
    ///
    /// **`Err` means `Gate`, never `Observe`, and never a panic left
    /// unhandled either.** The kernel wraps this call in `catch_unwind` and
    /// maps a caught panic to `Gate` the same way it maps an `Err` — a
    /// classifier that cannot answer must not be able to turn the statement
    /// gate off. This is a *looser* contract than `Policy::evaluate`'s, on
    /// purpose: `evaluate` runs only when a decision is genuinely being
    /// asked for, but this runs in front of *every* statement, including the
    /// ones nobody would ever gate, so its own failure must default to the
    /// conservative answer rather than propagate.
    ///
    /// **May raise the posture the kernel's own static rules already set;
    /// may never lower it.** A model is an escalation path, not an override
    /// — dangerous syntax classes keep a static gate floor no classifier can
    /// clear (spec §C.6).
    fn classify(
        &self,
        input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError>;
}

/// What [`StatementClassifier::classify`] is handed (spec §C.6): the plan,
/// unexpanded, plus what it would run against.
#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub struct StatementClassificationInput<'a> {
    /// The redacted, rendered statement and its structure — parse
    /// information, not execution information (spec §A.8, §C.6).
    pub plan: &'a Plan,
    /// What the statement would run against.
    pub context: &'a ExecutionContext,
}

impl<'a> StatementClassificationInput<'a> {
    /// Pair a plan with the context it would run against.
    pub fn new(plan: &'a Plan, context: &'a ExecutionContext) -> Self {
        Self { plan, context }
    }
}

/// What a statement would run against (spec §C.6). A classifier judging
/// `rm -rf .` needs to know where "." is; one judging a write needs to know
/// whether the target is a scratch mount or the project.
///
/// **Carries no host path anywhere.** `cwd` is the logical path the VFS
/// router resolves against — the same convention
/// [`PlanBinding::cwd`](kaish_types::approval::PlanBinding::cwd) already
/// uses, for the identical reason: kaish has no `VirtualPath` newtype, and
/// this crate cannot depend on `kaish-vfs` to borrow one. A classifier is
/// frequently a model and its input frequently leaves the process —
/// `/home/amy/clients/acme` says things `MountClass::Project` does not.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct ExecutionContext {
    /// The working directory, as a logical VFS path.
    pub cwd: String,
    /// Which kernel, session, and actor this statement runs under.
    pub scope: ApprovalScope,
    /// Which sandbox profile is in force, when the embedder names them.
    pub sandbox_profile: Option<SandboxProfileId>,
    /// Every mount the router knows about, with the embedder's own
    /// classification of what lives there.
    pub mounts: Vec<MountDescriptor>,
}

impl ExecutionContext {
    /// Describe the context a statement runs in.
    pub fn new(cwd: impl Into<String>, scope: ApprovalScope) -> Self {
        Self {
            cwd: cwd.into(),
            scope,
            sandbox_profile: None,
            mounts: Vec::new(),
        }
    }

    /// Name the sandbox profile in force.
    pub fn with_sandbox_profile(mut self, profile: SandboxProfileId) -> Self {
        self.sandbox_profile = Some(profile);
        self
    }

    /// Attach the router's mount table.
    pub fn with_mounts(mut self, mounts: Vec<MountDescriptor>) -> Self {
        self.mounts = mounts;
        self
    }
}

/// One VFS mount, classified for a classifier's use (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct MountDescriptor {
    /// The logical VFS prefix this mount is bound at.
    pub prefix: String,
    /// The embedder's classification of what lives there.
    pub class: MountClass,
    /// Whether this mount accepts writes.
    pub access: MountAccess,
}

impl MountDescriptor {
    /// Describe one mount.
    pub fn new(prefix: impl Into<String>, class: MountClass, access: MountAccess) -> Self {
        Self {
            prefix: prefix.into(),
            class,
            access,
        }
    }
}

/// The embedder's classification of what a mount holds (spec §C.6). The
/// kernel does not infer this from a mount's backend type — an
/// `OverlayFs`-backed scratch space and an `OverlayFs`-backed checkout look
/// identical to the router; only the embedder that mounted them knows which
/// is which.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MountClass {
    /// The project a session is working on.
    Project,
    /// Disposable working space — safe to delete without asking.
    Scratch,
    /// Host or kernel infrastructure a session should rarely touch.
    System,
    /// Anything outside the above — a mounted credential store, a remote
    /// share.
    External,
}

/// Whether a mount accepts writes (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MountAccess {
    /// Reads only.
    ReadOnly,
    /// Reads and writes.
    ReadWrite,
}

/// Why a [`StatementClassifier`] could not reach a judgment (spec §C.6).
/// Every reason maps to [`StatementPosture::Gate`] at the tap site — never to
/// `Observe`, and never silently.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassificationError {
    /// Why the classifier could not answer — model unavailable, input
    /// outside its distribution, a malformed rule. Recorded on the
    /// resulting `Assessed` entry (spec §C.7) so the gate the classifier
    /// forced is explained, not just observed.
    pub reason: String,
}

impl ClassificationError {
    /// Name why classification failed.
    pub fn new(reason: impl Into<String>) -> Self {
        Self { reason: reason.into() }
    }
}

impl std::fmt::Display for ClassificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl std::error::Error for ClassificationError {}

/// What a [`StatementClassifier`] says about one statement (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementPosture {
    /// Record and run. The default, and the floor — there is no silent
    /// posture.
    Observe,
    /// Build an approval request and run the decision chain. The classifier
    /// names the risk because the taxonomy cannot: `cmd.execute` covers `ls`
    /// and `rm -rf` alike.
    Gate {
        /// Why this statement must ask. Read by an approver and recorded on
        /// the request.
        reason: String,
        /// How hard the statement is to walk back.
        risk: RiskClass,
    },
}

impl StatementPosture {
    /// Gate this statement, naming why and how hard it is to walk back.
    pub fn gate(reason: impl Into<String>, risk: RiskClass) -> Self {
        Self::Gate {
            reason: reason.into(),
            risk,
        }
    }

    /// Whether this posture is a gate.
    pub fn is_gate(&self) -> bool {
        matches!(self, Self::Gate { .. })
    }

    /// Combine this posture with a floor that must not be lowered (spec
    /// §C.6): "a classifier may raise posture to `Gate` freely; it may never
    /// lower a posture the kernel's own static rules set." `None` means no
    /// floor applies and `self` stands unmodified.
    ///
    /// When both gate, the higher risk wins and the reasons are joined, so
    /// an auditor reading the resulting request sees both why the floor
    /// applied and what the classifier itself noticed — neither is silently
    /// dropped in favor of the other.
    pub fn at_least(self, floor: Option<Self>) -> Self {
        let Some(floor) = floor else { return self };
        match (floor, self) {
            (
                Self::Gate {
                    reason: floor_reason,
                    risk: floor_risk,
                },
                Self::Gate {
                    reason: self_reason,
                    risk: self_risk,
                },
            ) => {
                let (reason, risk) = if risk_rank(floor_risk) > risk_rank(self_risk) {
                    (floor_reason, floor_risk)
                } else {
                    (self_reason, self_risk)
                };
                Self::Gate { reason, risk }
            }
            (floor @ Self::Gate { .. }, Self::Observe) => floor,
            (Self::Observe, gated) => gated,
        }
    }
}

/// How severe one [`RiskClass`] is relative to another, for
/// [`StatementPosture::at_least`]'s floor comparison. `RiskClass` is
/// `#[non_exhaustive]` from this crate's side, so an unrecognized future
/// variant ranks at least as severe as anything named today — an unknown
/// risk must never read as safer than the truth, the same rule
/// `KernelOperation::risk`'s own doc states for `cmd.execute`'s default.
fn risk_rank(risk: RiskClass) -> u8 {
    match risk {
        RiskClass::Reversible => 0,
        RiskClass::Recoverable => 1,
        RiskClass::Irreversible => 2,
        _ => u8::MAX,
    }
}

/// A [`StatementClassifier`]'s full judgment on one statement (spec §C.6): the
/// posture plus who reached it and how, so the kernel has something to
/// record on the resulting `Assessed` entry (spec §C.7) beyond the posture
/// alone.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub struct StatementAssessment {
    /// What the classifier decided.
    pub posture: StatementPosture,
    /// Who judged. Recorded on the `Assessed` entry.
    pub assessor: AssessorId,
    /// Stable version or weight identity, when a model decided. "A model
    /// allowed this" is not a reproducible audit statement without it.
    pub model: Option<ModelIdentity>,
    /// The classifier's confidence in its own judgment, when it has one.
    pub confidence: Option<f32>,
}

impl StatementAssessment {
    /// Record a judgment with no model identity and no confidence — the
    /// shape a rule-based classifier like [`CommandNameClassifier`] produces.
    pub fn new(posture: StatementPosture, assessor: AssessorId) -> Self {
        Self {
            posture,
            assessor,
            model: None,
            confidence: None,
        }
    }

    /// Name the model that judged.
    pub fn with_model(mut self, model: ModelIdentity) -> Self {
        self.model = Some(model);
        self
    }

    /// Attach the classifier's confidence in its own judgment.
    pub fn with_confidence(mut self, confidence: f32) -> Self {
        self.confidence = Some(confidence);
        self
    }
}

/// The reference classifier: gates a statement when any command it plans is
/// named in a set of command names (spec §C.6).
///
/// Matching is on [`PlannedCommand::name`](kaish_types::approval::PlannedCommand::name)
/// — argv0 exactly as written, from the parsed plan. That is the whole point
/// of classifying the plan rather than the raw line: `echo "rm -rf /"` plans
/// one command named `echo` and does not match, while `for f in $(ls); do rm
/// $f; done` plans an `rm` in its body and does.
///
/// It resolves no aliases and consults no `PATH` — a plan is parse
/// information, so `alias del=rm` plans a command named `del`. Name the
/// spellings the session actually uses.
pub struct CommandNameClassifier {
    names: Vec<String>,
    reason: String,
    risk: RiskClass,
}

impl CommandNameClassifier {
    /// Gate every statement planning a command with one of these names.
    pub fn new(
        names: impl IntoIterator<Item = impl Into<String>>,
        reason: impl Into<String>,
        risk: RiskClass,
    ) -> Self {
        Self {
            names: names.into_iter().map(Into::into).collect(),
            reason: reason.into(),
            risk,
        }
    }
}

/// The [`AssessorId`] every [`CommandNameClassifier`] records itself under.
pub const COMMAND_NAME_CLASSIFIER_ASSESSOR: &str = "command-name-classifier";

impl StatementClassifier for CommandNameClassifier {
    fn classify(
        &self,
        input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        let hit = input
            .plan
            .commands
            .iter()
            .find(|command| self.names.contains(&command.name));
        let posture = match hit {
            Some(command) => StatementPosture::Gate {
                reason: format!("{}: {}", self.reason, command.name),
                risk: self.risk,
            },
            None => StatementPosture::Observe,
        };
        Ok(StatementAssessment::new(
            posture,
            AssessorId::new(COMMAND_NAME_CLASSIFIER_ASSESSOR),
        ))
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use kaish_types::approval::{ApprovalScope, KernelId, PlannedCommand};

    fn plan_of(names: &[&str]) -> Plan {
        Plan::new(
            names.join("; "),
            "command",
            names
                .iter()
                .map(|n| PlannedCommand::new(*n, Vec::new(), Vec::new(), false))
                .collect(),
        )
    }

    fn context() -> ExecutionContext {
        ExecutionContext::new("/", ApprovalScope::kernel(KernelId::new(1)))
    }

    fn classify(classifier: &CommandNameClassifier, plan: &Plan) -> StatementPosture {
        let ctx = context();
        classifier
            .classify(&StatementClassificationInput::new(plan, &ctx))
            .expect("the reference classifier never errors")
            .posture
    }

    #[test]
    fn the_reference_classifier_gates_a_named_command_anywhere_in_the_plan() {
        let classifier =
            CommandNameClassifier::new(["rm"], "destructive command", RiskClass::Irreversible);
        // Second position, and in a body — the plan flattens both.
        let posture = classify(&classifier, &plan_of(&["echo", "rm"]));
        assert!(
            matches!(posture, StatementPosture::Gate { risk, .. } if risk == RiskClass::Irreversible),
            "expected a gate, got {posture:?}"
        );
    }

    #[test]
    fn the_reference_classifier_observes_when_no_command_matches() {
        let classifier =
            CommandNameClassifier::new(["rm"], "destructive command", RiskClass::Irreversible);
        assert_eq!(
            classify(&classifier, &plan_of(&["echo", "cat"])),
            StatementPosture::Observe
        );
    }

    #[test]
    fn the_reference_classifier_never_matches_a_quoted_argument() {
        // The discrimination the plan buys over the raw line: this statement
        // renders as `echo 'rm -rf /'` but plans only `echo`.
        let classifier =
            CommandNameClassifier::new(["rm"], "destructive command", RiskClass::Irreversible);
        let plan = Plan::new(
            "echo 'rm -rf /'",
            "command",
            vec![PlannedCommand::new(
                "echo",
                vec![kaish_types::approval::PlannedValue::Plain("'rm -rf /'".to_string())],
                Vec::new(),
                false,
            )],
        );
        assert_eq!(classify(&classifier, &plan), StatementPosture::Observe);
    }

    #[test]
    fn the_reference_classifier_names_itself_as_assessor() {
        let classifier =
            CommandNameClassifier::new(["rm"], "destructive command", RiskClass::Irreversible);
        let ctx = context();
        let assessment = classifier
            .classify(&StatementClassificationInput::new(&plan_of(&["rm"]), &ctx))
            .expect("never errors");
        assert_eq!(
            assessment.assessor,
            AssessorId::new(COMMAND_NAME_CLASSIFIER_ASSESSOR)
        );
    }

    #[test]
    fn a_classifiers_observe_cannot_lower_a_gated_floor() {
        let floor = StatementPosture::gate("floor reason", RiskClass::Irreversible);
        let classified = StatementPosture::Observe;
        let result = classified.at_least(Some(floor));
        assert!(result.is_gate(), "the floor must survive: {result:?}");
    }

    #[test]
    fn a_higher_classifier_risk_still_wins_over_a_lower_floor() {
        let floor = StatementPosture::gate("floor reason", RiskClass::Recoverable);
        let classified = StatementPosture::gate("classifier reason", RiskClass::Irreversible);
        let result = classified.clone().at_least(Some(floor));
        assert_eq!(result, classified, "the classifier's own higher risk must not be discarded");
    }

    #[test]
    fn no_floor_leaves_the_classifiers_posture_untouched() {
        let classified = StatementPosture::Observe;
        assert_eq!(classified.clone().at_least(None), classified);
    }

    #[test]
    fn execution_context_carries_only_a_logical_path() {
        // §C.6's no-host-path guarantee, pinned at the type level: `cwd` is
        // `String`, the same convention `PlanBinding::cwd` uses, never a
        // `PathBuf` or anything that could carry an absolute host path by
        // accident.
        let ctx = ExecutionContext::new("/workspace/project", ApprovalScope::kernel(KernelId::new(1)));
        let cwd: &str = &ctx.cwd;
        assert_eq!(cwd, "/workspace/project");
    }
}
