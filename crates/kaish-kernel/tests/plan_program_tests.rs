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

use kaish_kernel::ledger::CommandNameClassifier;
use kaish_kernel::{plan_program, Kernel, KernelConfig};
use kaish_types::approval::{PlanDigest, ResumeAction, RiskClass};
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
    let plans = plan_program("echo before\nrm f.txt\necho after").expect("parses");
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

/// The plan names the session variables a statement reads, and the peek
/// loop the metadata surface promises works against live state: plan, then
/// `get_var` each free variable, and judge with the value in hand.
#[tokio::test]
async fn free_variables_feed_the_get_var_peek_loop() {
    let dir = tempdir();
    let kernel = gating_kernel(dir.path());
    kernel
        .execute("TARGET=precious.txt")
        .await
        .expect("assignment");

    let plans = kernel.plan_program("rm ${TARGET}").expect("plans");
    assert_eq!(plans[0].plan.free_variables, vec!["TARGET".to_string()]);
    assert!(plans[0].plan.bound_variables.is_empty());

    let value = kernel
        .get_var(&plans[0].plan.free_variables[0])
        .await
        .expect("TARGET is set");
    assert_eq!(value, kaish_types::Value::String("precious.txt".to_string()));
}

/// A name the statement itself binds is never listed free — peeking session
/// state for a `for` variable would judge the statement against a value the
/// statement replaces.
#[test]
fn bound_names_never_read_as_free() {
    let plans =
        plan_program("for f in $(ls ${DIR}); do rm $f; done").expect("parses");
    assert_eq!(plans[0].plan.free_variables, vec!["DIR".to_string()]);
    assert_eq!(plans[0].plan.bound_variables, vec!["f".to_string()]);
}
