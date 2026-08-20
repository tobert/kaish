//! `plan` — the statement projection, reachable from a kaish body.
//!
//! `Kernel::plan_program` and the CLI's `--plan` flag have emitted this shape
//! since 0.15.0, but only to Rust callers and to the command line. An embedder
//! whose hooks are written *in kaish* — kaijutsu's PreCall bodies are — could
//! not reach it, and was reduced to parsing with `kaish_kernel::parser::parse`
//! and passing the result in through a variable. That puts the shape in the
//! embedder instead of in kaish, and every other hook author would repeat it.
//!
//! The rule under test: `plan` emits the same projection the CLI does, it never
//! executes anything, and it descends into places a line-splitter cannot see.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

async fn run(source: &str) -> (i64, String, String) {
    let k = kernel();
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().into_owned(), r.err.clone())
}

async fn plan_json(source: &str) -> serde_json::Value {
    let (code, out, err) = run(source).await;
    assert_eq!(code, 0, "plan should succeed: {err:?}");
    serde_json::from_str(&out).unwrap_or_else(|e| panic!("plan --json must emit JSON: {e}: {out:?}"))
}

/// The success shape is the CLI's: one object with `statements`.
#[tokio::test]
async fn plan_emits_the_statements_projection() {
    let doc = plan_json(r#"plan 'echo one; echo two' --json"#).await;
    let statements = doc["statements"].as_array().expect("statements array");
    assert_eq!(statements.len(), 2, "two top-level statements: {doc}");
    assert_eq!(statements[0]["index"], 0);
    assert_eq!(statements[0]["plan"]["rendered"], "echo one");
    assert_eq!(statements[1]["plan"]["rendered"], "echo two");
}

/// The reason this beats splitting on `;` or newlines: a command inside a loop
/// body is its own entry. A classifier scoring whole statements dilutes it;
/// scoring `commands[]` does not.
#[tokio::test]
async fn commands_descend_into_a_loop_body() {
    let doc = plan_json(r#"plan 'for f in a b; do shred $f; done | wc -l' --json"#).await;
    let names: Vec<&str> = doc["statements"][0]["plan"]["commands"]
        .as_array()
        .expect("commands")
        .iter()
        .map(|c| c["name"].as_str().unwrap_or_default())
        .collect();
    assert!(
        names.contains(&"shred"),
        "a command inside the loop body must surface on its own: {names:?}"
    );
    assert!(names.contains(&"wc"), "and so must the later pipeline stage: {names:?}");
}

/// Planning is not running. If `plan` executed its argument, the file would
/// exist and the check after it would find it.
///
/// The control half is not decoration. The first version of this test used
/// `/planned.txt`, which `touch` cannot create in an isolated kernel — so it
/// reported INERT on a permission error and would have passed just as happily
/// against a `plan` that executed everything. The control proves the path is
/// writable before the absence of the file is allowed to mean anything.
#[tokio::test]
async fn plan_does_not_execute_what_it_plans() {
    let (_, control, err) = run(
        r#"touch /v/control.txt; if [[ -e /v/control.txt ]]; then echo CREATED; else echo NO; fi"#,
    )
    .await;
    assert_eq!(
        control.trim(),
        "CREATED",
        "control: touch must work at this path, or the assertion below is vacuous: {err:?}"
    );

    let (code, out, _) = run(
        r#"plan 'touch /v/planned.txt' > /dev/null; if [[ -e /v/planned.txt ]]; then echo RAN; else echo INERT; fi"#,
    )
    .await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "INERT", "plan must not execute its argument");
}

/// A parse error is exit 2 — the usage code the CLI uses for the same input —
/// and says what was wrong rather than emitting an empty success.
#[tokio::test]
async fn a_parse_error_exits_2_and_explains() {
    let (code, _, err) = run(r#"plan 'for f in'"#).await;
    assert_eq!(code, 2, "parse failure is the usage code");
    assert!(!err.is_empty(), "a parse failure must say what was wrong");
}

/// Reads stdin when no source argument is given, so a body can pipe a
/// statement in rather than quoting it into argv.
#[tokio::test]
async fn plan_reads_stdin_when_given_no_argument() {
    let doc = plan_json(r#"echo 'echo hi' | plan --json"#).await;
    assert_eq!(doc["statements"][0]["plan"]["rendered"], "echo hi");
}

/// Free and bound variables ride along, so a hook can resolve what a statement
/// depends on before deciding about it.
#[tokio::test]
async fn plan_reports_the_variables_a_statement_reads_and_writes() {
    let doc = plan_json(r#"plan 'd=/tmp; echo "$d"' --json"#).await;
    let bound = doc["statements"][0]["plan"]["bound_variables"]
        .as_array()
        .expect("bound_variables");
    assert!(bound.iter().any(|v| v == "d"), "assignment binds d: {doc}");
    let free = doc["statements"][1]["plan"]["free_variables"]
        .as_array()
        .expect("free_variables");
    assert!(free.iter().any(|v| v == "d"), "the echo reads d: {doc}");
}
