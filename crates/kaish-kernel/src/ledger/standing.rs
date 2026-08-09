//! Standing-grant matching (`docs/approval-ledger.md` §C.4).
//!
//! Pure and lock-free: this module decides *whether* a rule covers a request
//! and nothing else. Consuming `max_uses` and appending the `Granted` entry
//! belong to the one critical section in `core.rs` (spec §B.1), which calls
//! [`matches`] while it holds the lock — glob matching is CPU work with no
//! I/O and no `.await`, which is the only reason stage 1 of the decision
//! chain is allowed to run under the lock at all.
//!
//! The rules, all chosen for loudness (spec §C.4):
//!
//! - **All-or-nothing.** Every resource on the request must be covered by
//!   some pattern. A rule covering three of four resources does not match —
//!   it does not auto-approve the three and gate the one.
//! - **Kind matches exactly**; only `id` globs, via `kaish-glob`, so the
//!   semantics are the ones the rest of kaish already uses. Both pattern
//!   primitives live in [`super::patterns`], shared with subscriptions.
//! - **Transitions are not matched, they are conditioned.** Matching ignores
//!   the declared oids entirely; the caller copies them into the resulting
//!   grant's conditions so the redemption-time check still fires.

use std::time::SystemTime;

use kaish_types::approval::{ApprovalRequest, StandingGrant};

use super::patterns::{covers_operation, covers_resource};

/// Whether `rule` covers `request` as of `now`.
///
/// Fails closed at every boundary: an expired rule matches nothing, a rule
/// naming no operation matches nothing, and a rule whose resource patterns
/// leave one of the request's resources uncovered matches nothing.
pub(crate) fn matches(rule: &StandingGrant, request: &ApprovalRequest, now: SystemTime) -> bool {
    if rule.expires_at.is_some_and(|expires_at| now >= expires_at) {
        return false;
    }
    if let Some(required) = &rule.principal {
        if required != &request.principal {
            return false;
        }
    }
    // A rule that names no operation is inert rather than universal (see
    // `super::patterns`).
    if !covers_operation(&rule.operations, &request.operation) {
        return false;
    }
    // All-or-nothing, with set semantics: every resource needs *some*
    // covering pattern, one pattern may cover several resources, and
    // duplicate resources impose no extra requirement. A request with no
    // resources is covered vacuously.
    request
        .resources
        .iter()
        .all(|resource| covers_resource(&rule.resources, resource))
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use kaish_types::approval::{
        ApprovalScope, Capture, KernelId, OperationPattern, PlanBinding, PlanDigest, Principal,
        PrincipalKind, RequestId, RequestOrigin, ResourcePattern, Resource, RiskClass, StateClaim,
    };
    use std::time::Duration;

    fn request(op: &str, resources: Vec<Resource>) -> ApprovalRequest {
        let mut builder = ApprovalRequest::builder(op).risk(RiskClass::Reversible);
        for resource in resources {
            builder = builder.resource(resource);
        }
        let scope = ApprovalScope::kernel(KernelId::new(1));
        builder.build().unwrap().stamp(
            RequestId::new(1, 1),
            SystemTime::UNIX_EPOCH,
            RequestOrigin::new(
                scope.clone(),
                PlanBinding::new(PlanDigest::new("test"), "/", scope),
                Principal::new("agent-1", PrincipalKind::Agent),
                Capture::DirectExecution,
                Duration::from_secs(60),
            ),
        )
    }

    fn rule(operations: &[&str], resources: &[(&str, &str)]) -> StandingGrant {
        // max_uses is irrelevant to pure matching (consumption is core.rs's
        // job), so the one-shot default is fine here.
        StandingGrant::new(
            operations.iter().map(|o| OperationPattern::new(*o)).collect(),
            resources
                .iter()
                .map(|(kind, pattern)| ResourcePattern::new(*kind, *pattern))
                .collect(),
            None,
            None,
            Principal::new("operator", PrincipalKind::Human),
            "test rule",
        )
    }

    fn now() -> SystemTime {
        SystemTime::UNIX_EPOCH + Duration::from_secs(1_000)
    }

    #[test]
    fn kind_must_match_exactly_and_only_the_id_globs() {
        let rule = rule(&["git.commit"], &[("git.ref", "refs/heads/agent/*")]);
        let covered = request(
            "git.commit",
            vec![Resource::plain("git.ref", "refs/heads/agent/work")],
        );
        assert!(matches(&rule, &covered, now()));

        // Same id, different kind — the kind is never globbed, so this is a
        // miss even though the id would match.
        let wrong_kind = request("git.commit", vec![Resource::plain("path", "refs/heads/agent/work")]);
        assert!(!matches(&rule, &wrong_kind, now()));

        // Same kind, id outside the glob.
        let wrong_id = request("git.commit", vec![Resource::plain("git.ref", "refs/heads/main")]);
        assert!(!matches(&rule, &wrong_id, now()));
    }

    #[test]
    fn a_glob_in_the_kind_position_is_a_literal_kind_not_a_wildcard() {
        let rule = rule(&["git.commit"], &[("*", "*")]);
        let req = request("git.commit", vec![Resource::plain("git.ref", "refs/heads/main")]);
        assert!(!matches(&rule, &req, now()));
    }

    #[test]
    fn every_resource_must_be_covered_or_nothing_matches() {
        let rule = rule(&["fs.write"], &[("path", "/workspace/*")]);
        let three_covered_one_not = request(
            "fs.write",
            vec![
                Resource::plain("path", "/workspace/a"),
                Resource::plain("path", "/workspace/b"),
                Resource::plain("path", "/workspace/c"),
                Resource::plain("path", "/etc/passwd"),
            ],
        );
        assert!(!matches(&rule, &three_covered_one_not, now()));

        let all_covered = request(
            "fs.write",
            vec![
                Resource::plain("path", "/workspace/a"),
                Resource::plain("path", "/workspace/b"),
            ],
        );
        assert!(matches(&rule, &all_covered, now()));
    }

    #[test]
    fn one_pattern_may_cover_several_resources_and_duplicates_cost_nothing() {
        let rule = rule(&["fs.write"], &[("path", "/workspace/*")]);
        let req = request(
            "fs.write",
            vec![
                Resource::plain("path", "/workspace/a"),
                Resource::plain("path", "/workspace/a"),
                Resource::plain("path", "/workspace/b"),
            ],
        );
        assert!(matches(&rule, &req, now()));
    }

    #[test]
    fn transitions_are_ignored_by_matching() {
        let rule = rule(&["git.commit"], &[("git.ref", "refs/heads/agent/*")]);
        let req = request(
            "git.commit",
            vec![Resource::transition(
                "git.ref",
                "refs/heads/agent/work",
                StateClaim::Exact("a1b2c3d".into()),
                StateClaim::Exact("c3d4e5f".into()),
            )],
        );
        assert!(matches(&rule, &req, now()));
    }

    #[test]
    fn an_expired_rule_matches_nothing() {
        let mut rule = rule(&["fs.write"], &[("path", "/workspace/*")]);
        rule.expires_at = Some(now() - Duration::from_secs(1));
        let req = request("fs.write", vec![Resource::plain("path", "/workspace/a")]);
        assert!(!matches(&rule, &req, now()));

        rule.expires_at = Some(now() + Duration::from_secs(1));
        assert!(matches(&rule, &req, now()));
    }

    #[test]
    fn a_rule_naming_no_operation_matches_nothing() {
        let rule = rule(&[], &[("path", "*")]);
        let req = request("fs.write", vec![Resource::plain("path", "/workspace/a")]);
        assert!(!matches(&rule, &req, now()));
    }

    #[test]
    fn a_rule_with_no_resource_patterns_covers_only_a_resourceless_request() {
        let rule = rule(&["trash.empty"], &[]);
        assert!(matches(&rule, &request("trash.empty", vec![]), now()));
        assert!(!matches(
            &rule,
            &request("trash.empty", vec![Resource::plain("path", "/x")]),
            now()
        ));
    }

    #[test]
    fn a_principal_scoped_rule_matches_only_that_principal() {
        let mut rule = rule(&["fs.write"], &[("path", "*")]);
        let req = request("fs.write", vec![Resource::plain("path", "/x")]);
        rule.principal = Some(Principal::new("agent-1", PrincipalKind::Agent));
        assert!(matches(&rule, &req, now()));

        // Same id, different kind — `Principal` equality is whole-value.
        rule.principal = Some(Principal::new("agent-1", PrincipalKind::Human));
        assert!(!matches(&rule, &req, now()));

        rule.principal = Some(Principal::new("agent-2", PrincipalKind::Agent));
        assert!(!matches(&rule, &req, now()));
    }

    #[test]
    fn an_operation_glob_covers_a_namespace() {
        let rule = rule(&["fs.*"], &[("path", "*")]);
        let req = request("fs.remove", vec![Resource::plain("path", "/x")]);
        assert!(matches(&rule, &req, now()));
        let other = request("git.push", vec![Resource::plain("path", "/x")]);
        assert!(!matches(&rule, &other, now()));
    }
}
