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

use kaish_types::approval::{Plan, RiskClass};

/// Decides which top-level statements must ask before they run (spec §C.6).
///
/// Register one with `KernelConfig::with_statement_classifier`. With none
/// registered every statement is [`StatementPosture::Observe`] — recorded and
/// run.
pub trait StatementClassifier: Send + Sync {
    /// Classify one top-level statement from its plan.
    ///
    /// **Synchronous and non-blocking**, like `Policy::evaluate` (spec
    /// §C.2): it runs on the execution path of every statement, before
    /// anything of the statement has run. A classifier too slow for that
    /// path returns `Gate`, and the expensive judgment happens out of band
    /// after the statement returns exit 2 — which is where every slow
    /// decision lives; the kernel awaits none of them.
    ///
    /// **It must not panic, and a panic propagates.** kaish installs no
    /// `catch_unwind` here — the same contract `Policy::evaluate`
    /// carries, and for the same reason: a hook that panics
    /// is an embedder bug, and swallowing it would run the statement under a
    /// posture nothing decided. The unwind corrupts nothing (an in-flight
    /// attempt settles through its drop guard, and the kernel's locks do not
    /// poison), so the loud outcome is the honest one. Return
    /// [`StatementPosture::Observe`] for input a classifier does not
    /// understand.
    fn classify(&self, plan: &Plan) -> StatementPosture;
}

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

impl StatementClassifier for CommandNameClassifier {
    fn classify(&self, plan: &Plan) -> StatementPosture {
        let hit = plan
            .commands
            .iter()
            .find(|command| self.names.contains(&command.name));
        match hit {
            Some(command) => StatementPosture::Gate {
                reason: format!("{}: {}", self.reason, command.name),
                risk: self.risk,
            },
            None => StatementPosture::Observe,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use kaish_types::approval::PlannedCommand;

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

    #[test]
    fn the_reference_classifier_gates_a_named_command_anywhere_in_the_plan() {
        let classifier =
            CommandNameClassifier::new(["rm"], "destructive command", RiskClass::Irreversible);
        // Second position, and in a body — the plan flattens both.
        let posture = classifier.classify(&plan_of(&["echo", "rm"]));
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
            classifier.classify(&plan_of(&["echo", "cat"])),
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
        assert_eq!(classifier.classify(&plan), StatementPosture::Observe);
    }
}
