//! `plan_program`: the statement-metadata surface (spike for the ledger
//! path-C question). An embedder reads a program's plans without executing
//! anything, and what it reads correlates with what the kernel's own gate
//! records for the same source — same indexes, same rendered text, same
//! binding digest, same redaction.
//!
//! Everything below the metadata read drives real source through
//! `kernel.execute()`, so the correlation claims are checked against the
//! full pipeline, not against a second copy of the planning walk.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::sync::Arc;

use kaish_kernel::ledger::{CommandNameClassifier, RedactionMark, Redactor};
use kaish_kernel::{plan_program, Kernel, KernelConfig};
use kaish_types::approval::{PlanDigest, ResumeAction, RiskClass, ValueSite};
use sha2::{Digest, Sha256};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("plan-program-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel whose statement gate holds `rm`, in its own directory.
fn gating_kernel(dir: &std::path::Path) -> Kernel {
    let (kernel, _authority) = Kernel::build(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_statement_classifier(Arc::new(CommandNameClassifier::new(
                ["rm"],
                "the statement plans a destructive command",
                RiskClass::Irreversible,
            ))),
    )
    .expect("kernel");
    kernel
}

/// The correlation the metadata surface promises: the `index` a plan carries
/// is the `index` the kernel's gate quotes when it holds the same statement,
/// and the binding digest the grant would be judged under is computable from
/// the metadata alone — SHA-256 of the rendered plan with confirm tokens
/// stripped (spec §A.9).
#[tokio::test]
async fn a_held_statement_quotes_the_planned_index_and_digest() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "keep me").expect("write");
    let kernel = gating_kernel(dir.path());

    let source = "echo before\nrm f.txt\necho after";
    let plans = kernel.plan_program(source).expect("the source parses");
    assert_eq!(plans.len(), 3);
    let held = &plans[1];
    assert_eq!(held.plan.commands[0].name, "rm");
    assert_eq!(held.plan.rendered, "rm f.txt");

    let result = kernel.execute(source).await.expect("execute");
    assert_eq!(result.code, 2, "the rm statement is held: {}", result.err);
    let pending = result
        .approval
        .expect("a held statement carries its pending request");
    match &pending.resume {
        ResumeAction::ConfirmStatement { plan_digest, index } => {
            assert_eq!(
                *index, held.index,
                "the gate quotes the planned statement's own index"
            );
            let judged =
                kaish_kernel::ast::plan::strip_confirm_tokens(&held.plan.rendered);
            let expected = PlanDigest::new(format!("{:x}", Sha256::digest(judged.as_bytes())));
            assert_eq!(
                plan_digest, &expected,
                "the binding digest is computable from the plan alone"
            );
        }
        other => panic!("a held statement resumes by ConfirmStatement, got {other:?}"),
    }
}

/// The free function needs no kernel at all: same source, same plans, no
/// filesystem and no ledger anywhere in sight.
#[test]
fn plan_program_needs_no_kernel() {
    let plans = plan_program("echo before\nrm f.txt\necho after", None).expect("parses");
    assert_eq!(
        plans
            .iter()
            .map(|p| (p.index, p.plan.rendered.as_str()))
            .collect::<Vec<_>>(),
        vec![(0, "echo before"), (1, "rm f.txt"), (2, "echo after")],
    );
}

/// Nothing executes during planning: the statement that would delete the
/// file is planned, and the file is still there.
#[tokio::test]
async fn planning_executes_nothing() {
    let dir = tempdir();
    let target = dir.path().join("f.txt");
    std::fs::write(&target, "keep me").expect("write");
    let kernel = gating_kernel(dir.path());

    let plans = kernel.plan_program("rm f.txt").expect("plans");
    assert_eq!(plans[0].plan.commands[0].name, "rm");
    assert!(target.exists(), "planning must not run the statement");
}

/// A redactor marks one secret value.
struct MarksOneSecret;

impl Redactor for MarksOneSecret {
    fn redact(&self, value: &str, _site: ValueSite) -> Option<RedactionMark> {
        (value == "hunter2").then(|| RedactionMark::new("test-secret"))
    }
}

/// `Kernel::plan_program` reads through the kernel's installed redactor —
/// the same seam the gate and the record read through (spec §A.8) — so an
/// embedder composing its own machinery over these plans inherits the same
/// redaction instead of needing its own.
#[tokio::test]
async fn the_kernel_method_applies_the_installed_redactor() {
    let dir = tempdir();
    let (kernel, _authority) = Kernel::build(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_redactor(Arc::new(MarksOneSecret)),
    )
    .expect("kernel");

    let plans = kernel
        .plan_program("deploy --password=hunter2 web")
        .expect("plans");
    assert!(
        !plans[0].plan.rendered.contains("hunter2"),
        "the secret must not survive into the plan: {}",
        plans[0].plan.rendered
    );

    // The free function with no redactor is the honest default: plain text.
    let plain = plan_program("deploy --password=hunter2 web", None).expect("plans");
    assert!(plain[0].plan.rendered.contains("hunter2"));
}
