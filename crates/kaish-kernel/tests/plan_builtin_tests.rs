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

/// The index a hook reads is the position in the list it got.
///
/// A script that opens with a comment is the common case, and it used to be
/// exactly the case where `index` and list position disagreed — so a hook that
/// reported "statement 2" pointed at statement 1.
#[tokio::test]
async fn indexes_match_list_position_even_with_a_leading_comment() {
    let doc = plan_json("plan '# lead\necho a\necho b' --json").await;
    let statements = doc["statements"].as_array().expect("statements");
    assert_eq!(statements.len(), 2, "the comment is not a statement: {doc}");
    for (position, statement) in statements.iter().enumerate() {
        assert_eq!(
            statement["index"].as_u64(),
            Some(position as u64),
            "index must equal list position: {doc}"
        );
    }
}

/// The failure that mattered most, because of what `plan` is for.
///
/// `plan rm build` used to plan `rm` with no arguments and discard the rest —
/// a hook scoring the answer would have seen a bare `rm` and judged a command
/// that was never asked about. Truncating the statement under analysis is worse
/// than refusing to analyze it, so this refuses and names the fix.
#[tokio::test]
async fn more_than_one_word_is_refused_rather_than_truncated() {
    let (code, out, err) = run("plan rm build").await;
    assert_eq!(code, 2, "extra words are a usage error, not a shorter plan");
    assert!(
        !out.contains("\"rm\"") && !out.contains("rendered"),
        "no plan may be emitted for a statement that was not fully given: {out:?}"
    );
    assert!(
        err.contains("quote the whole statement"),
        "the error must name the fix: {err:?}"
    );

    // The quoted form is the fix the message names, and it plans the whole
    // statement — arguments included.
    let doc = plan_json("plan 'rm build' --json").await;
    let args = doc["statements"][0]["plan"]["commands"][0]["args"]
        .as_array()
        .expect("args");
    assert_eq!(args.len(), 1, "the argument survives when quoted: {doc}");
}

/// An empty plan is where the builtin and the CLI diverge, pinned so the
/// divergence is a decision rather than a surprise.
///
/// `kaish --plan ''` prints `{"statements":[]}` because the CLI promises one
/// JSON object always. The builtin prints nothing, because the kernel's
/// `--json` contract leaves an empty success empty — the same rule that keeps
/// `grep`'s no-match quiet. The kernel rule wins inside the kernel.
#[tokio::test]
async fn an_empty_plan_stays_empty_under_the_kernel_json_rule() {
    let (code, out, _) = run("plan '' --json").await;
    assert_eq!(code, 0, "an empty statement is not an error");
    assert_eq!(out, "", "an empty success stays empty under --json");
}

/// A numeral argv word must round-trip its exact source text: `Plan.rendered`
/// and `PlannedCommand::args` are lexed into a typed `Int`/`Float` and were
/// re-serialized from the *value*, not the source. `i64`/`f64` have no
/// negative zero (`-0` → `0`) or memory of leading zeros (`007` → `7`) or
/// non-canonical trailing fraction digits (`1.0` → `1`), so every one of
/// these silently rewrote the word it was given.
///
/// `xargs -0 rm -f` is the case that matters: `-0` planned (and executed) as
/// a bare `0`, turning `xargs`'s idiomatic null-delimiter flag into a
/// positional argument with no error.
#[tokio::test]
async fn noncanonical_numeric_argv_words_round_trip_in_rendered() {
    let cases: &[(&str, &str)] = &[
        ("echo -0", "echo -0"),
        ("echo -00", "echo -00"),
        ("echo -0.0", "echo -0.0"),
        ("echo -0.00", "echo -0.00"),
        ("echo 00", "echo 00"),
        ("echo 007", "echo 007"),
        ("echo 010", "echo 010"),
        ("echo 0.10", "echo 0.10"),
        ("echo 1.0", "echo 1.0"),
        ("xargs -0 rm -f", "xargs -0 rm -f"),
    ];
    for (source, expected) in cases {
        let doc = plan_json(&format!("plan '{source}' --json")).await;
        assert_eq!(
            doc["statements"][0]["plan"]["rendered"], *expected,
            "rendered must reproduce the source word exactly for {source:?}: {doc}"
        );
    }
}

/// The same fidelity, checked on the structured `args[].plain` field —
/// `Plan.rendered` is a flat string a classifier might not re-split, but
/// `PlannedCommand::args` is what a hook is meant to read argument-by-argument.
#[tokio::test]
async fn noncanonical_numeric_argv_words_round_trip_in_args_plain() {
    let doc = plan_json("plan 'xargs -0 rm -f' --json").await;
    let args: Vec<&str> = doc["statements"][0]["plan"]["commands"][0]["args"]
        .as_array()
        .expect("args")
        .iter()
        .map(|a| a["plain"].as_str().unwrap_or_default())
        .collect();
    assert_eq!(
        args,
        vec!["-0", "rm", "-f"],
        "xargs -0 must survive as its own argv word, not become a bare 0: {doc}"
    );
}

/// Canonical numerals — the common case — must stay exactly as correct as
/// they were before: this class of fix must not touch a numeral whose
/// `Display` already reproduces the source.
#[tokio::test]
async fn canonical_numeric_argv_words_are_unaffected() {
    let cases: &[(&str, &str)] = &[
        ("echo -1", "echo -1"),
        ("echo -5", "echo -5"),
        ("echo -0.5", "echo -0.5"),
        ("echo +0", "echo +0"),
        ("echo +1", "echo +1"),
    ];
    for (source, expected) in cases {
        let doc = plan_json(&format!("plan '{source}' --json")).await;
        assert_eq!(
            doc["statements"][0]["plan"]["rendered"], *expected,
            "a canonical numeral must round-trip too: {source:?}: {doc}"
        );
    }
}
