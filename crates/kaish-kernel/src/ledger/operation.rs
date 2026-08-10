//! The kernel's operation taxonomy (`docs/approval-ledger.md` §A.6).
//!
//! In-tree operations come from a closed enum and the mapping to the dotted
//! id is an exhaustive match, so **adding a gate site without registering its
//! operation is a compile error**. Plugins get
//! [`OperationId::namespaced`](kaish_types::approval::OperationId::namespaced),
//! which refuses the reserved `fs.`/`trash.` prefixes — the `fs.` namespace
//! belongs to the kernel, and a policy engine's vocabulary stays honest
//! because of it.

use kaish_types::approval::{OperationId, Plan, RiskClass};

use kaish_tool_api::StatementPosture;

/// Every operation a kernel gate site can post. Closed by design: the
/// `id`/`risk` matches below are exhaustive, so a new gate site must name
/// itself here before it can request anything.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KernelOperation {
    /// `rm` removing a path permanently — the trash did not catch it.
    FsRemove,
    /// A truncating overwrite of an existing file (`cp`, `dd`, `patch`,
    /// `sed -i`, `tee`, `write`).
    FsOverwrite,
    /// `mv` replacing an existing destination.
    FsRename,
    /// `kaish-trash empty` discarding the recovery net itself.
    TrashEmpty,
    /// One top-level statement, recorded before it runs — the statement tap
    /// and the statement gate (spec §C.6).
    CmdExecute,
}

impl KernelOperation {
    /// The dotted id this operation posts under. Exhaustive by construction.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::FsRemove => "fs.remove",
            Self::FsOverwrite => "fs.overwrite",
            Self::FsRename => "fs.rename",
            Self::TrashEmpty => "trash.empty",
            Self::CmdExecute => "cmd.execute",
        }
    }

    /// How hard this operation is to walk back. An approver reads it and a
    /// policy matches on it; it carries no redemption policy of its own
    /// (spec §F.3 item 4 — every grant authorizes exactly one successful
    /// settlement, in every risk class alike).
    ///
    /// `cmd.execute` is the one operation whose real risk lives elsewhere:
    /// it covers `ls` and `rm -rf` alike, so the classifier names the risk on
    /// its `StatementPosture::Gate` and a statement request carries *that*
    /// (spec §C.6). The value here is the floor a caller gets when nobody
    /// named one, and it is the conservative end on purpose — an unnamed risk
    /// must never read as safer than the truth.
    pub const fn risk(self) -> RiskClass {
        match self {
            // The trash already caught everything it could — what reaches
            // these gates is the case with no recovery net left.
            Self::FsRemove
            | Self::FsOverwrite
            | Self::FsRename
            | Self::TrashEmpty
            | Self::CmdExecute => RiskClass::Irreversible,
        }
    }

    /// Whether this operation gates regardless of any subscription or policy
    /// (spec §F.1). Only `trash.empty` does: it discards the recovery net
    /// that makes every other `fs.*` operation survivable, so it is not
    /// something a session can turn off.
    pub const fn always_enforced(self) -> bool {
        matches!(self, Self::TrashEmpty)
    }

    /// The typed id, ready to draft against.
    pub fn id(self) -> OperationId {
        // Infallible: every string above is a well-formed dotted id, and this
        // is proven by `every_operation_builds_a_valid_id` below.
        OperationId::new(self.as_str()).unwrap_or_else(|e| {
            unreachable!("kernel operation {:?} is not a valid OperationId: {e}", self)
        })
    }
}

/// The kernel's static gate floor for one top-level statement (spec §C.6):
/// "dangerous syntax classes keep a static gate floor that no classifier can
/// clear." Combined with a *registered* classifier's own answer by
/// [`StatementPosture::at_least`] — a classifier may raise the resulting
/// posture further, never lower it below what this returns.
///
/// **Only consulted when a classifier is registered.** The rule this
/// function exists for is about bounding a classifier's answer — "a
/// classifier may raise... it may never lower" (spec §C.6) has a
/// classifier as its subject. A kernel with none registered keeps its
/// pre-R4 default: every statement is `Observe` at this layer, and
/// `kaish-trash empty` still gates at its own always-enforced `trash.empty`
/// site regardless (spec §F.1) — this floor is defense in depth layered
/// under an embedder's classifier, not a second, independent gate site.
///
/// **Scope for this PR**: seeded with exactly the one dangerous syntax class
/// the kernel already treats as unconditional at the `fs.*` layer
/// (`KernelOperation::always_enforced`, spec §F.1) — a `kaish-trash empty`
/// invocation, because it discards the recovery net every other `fs.*` gate
/// depends on. A broader static taxonomy ("recursive delete of `/`", generic
/// `rm -rf` detection) is real future work with no settled design in the spec
/// (§I has no ruling on it) — inventing one here would be scope creep this
/// lane was not asked to carry. `None` means no floor applies; the
/// classifier's own answer stands unmodified.
///
/// [`StatementClassifier`]: kaish_tool_api::StatementClassifier
pub(crate) fn static_gate_floor(plan: &Plan) -> Option<StatementPosture> {
    let empties_the_trash = plan.commands.iter().any(|command| {
        command.name == "kaish-trash"
            && command.args.first().is_some_and(|arg| match arg {
                kaish_types::approval::PlannedValue::Plain(s) => s == "empty",
                // A redacted first argument means this floor cannot rule out
                // `empty` — count it as a match rather than silently skipping
                // the one static floor this operation has. `PlannedValue` is
                // `#[non_exhaustive]`, so a variant this build does not know
                // about gets the same conservative answer.
                _ => true,
            })
    });
    empties_the_trash.then(|| {
        StatementPosture::gate(
            "kaish-trash empty discards the recovery net — no classifier may observe this away",
            KernelOperation::TrashEmpty.risk(),
        )
    })
}

impl std::fmt::Display for KernelOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use kaish_types::approval::PlannedCommand;

    fn plan_of(commands: Vec<PlannedCommand>) -> Plan {
        Plan::new("<test>", "command", commands)
    }

    #[test]
    fn kaish_trash_empty_hits_the_static_floor() {
        let plan = plan_of(vec![PlannedCommand::new(
            "kaish-trash",
            vec![kaish_types::approval::PlannedValue::Plain("empty".to_string())],
            Vec::new(),
            false,
        )]);
        let floor = static_gate_floor(&plan).expect("kaish-trash empty must set a floor");
        assert!(floor.is_gate());
        assert!(matches!(floor, StatementPosture::Gate { risk, .. } if risk == KernelOperation::TrashEmpty.risk()));
    }

    #[test]
    fn kaish_trash_list_sets_no_floor() {
        // Only `empty` discards the recovery net — every other subcommand is
        // ordinary reversible reading or bookkeeping.
        let plan = plan_of(vec![PlannedCommand::new(
            "kaish-trash",
            vec![kaish_types::approval::PlannedValue::Plain("list".to_string())],
            Vec::new(),
            false,
        )]);
        assert_eq!(static_gate_floor(&plan), None);
    }

    #[test]
    fn an_unrelated_statement_sets_no_floor() {
        let plan = plan_of(vec![PlannedCommand::new("echo", vec![kaish_types::approval::PlannedValue::Plain("hi".to_string())], Vec::new(), false)]);
        assert_eq!(static_gate_floor(&plan), None);
    }

    const ALL: &[KernelOperation] = &[
        KernelOperation::FsRemove,
        KernelOperation::FsOverwrite,
        KernelOperation::FsRename,
        KernelOperation::TrashEmpty,
        KernelOperation::CmdExecute,
    ];

    #[test]
    fn every_operation_builds_a_valid_id() {
        // `id()` claims infallibility. Prove it for every variant rather than
        // discovering a malformed id at a gate site under load.
        for op in ALL {
            assert_eq!(op.id().as_str(), op.as_str());
        }
    }

    #[test]
    fn every_operation_sits_in_a_kernel_reserved_namespace() {
        // The other half of §A.6: a plugin cannot post any of these, because
        // `OperationId::namespaced` refuses the prefixes they use.
        for op in ALL {
            let prefix = op.as_str().split('.').next().unwrap();
            assert!(
                OperationId::namespaced(prefix, "anything").is_err(),
                "{op} sits in a namespace a plugin can still claim"
            );
        }
    }

    #[test]
    fn only_trash_empty_is_always_enforced() {
        for op in ALL {
            assert_eq!(
                op.always_enforced(),
                *op == KernelOperation::TrashEmpty,
                "{op} disagrees with the always-enforced rule"
            );
        }
    }

    #[test]
    fn ids_are_distinct() {
        let mut ids: Vec<&str> = ALL.iter().map(|o| o.as_str()).collect();
        ids.sort_unstable();
        let before = ids.len();
        ids.dedup();
        assert_eq!(before, ids.len(), "two operations share one dotted id");
    }
}
