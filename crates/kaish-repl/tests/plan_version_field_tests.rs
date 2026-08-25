//! `kaish_version`, `kaish_git_hash`, and `kaish_build_date` are read from ONE
//! source in kaish-kernel (`kaish_kernel::{KAISH_VERSION, KAISH_GIT_HASH,
//! KAISH_BUILD_DATE}`) by TWO independent JSON emitters: this crate's
//! `kaish --plan`/`--plan-file`, and kaish-kernel's in-shell `plan` builtin.
//! Both are documented (CLAUDE.md, docs/EMBEDDING.md) to emit the same JSON.
//!
//! A test that checks each emitter against a hardcoded expected string does
//! not catch the two drifting apart: if someone later changes the constant
//! read by only one emitter, both hardcoded strings get updated to match and
//! the drift ships anyway. These tests compare the emitters' ACTUAL output to
//! EACH OTHER, so divergence itself is the failure, with no third value a
//! well-meaning edit can bring back into agreement.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::process::Command;

use kaish_kernel::{Kernel, KernelConfig};
use serde_json::Value;

/// The three fields under test, in the order the emitters write them.
const VERSION_FIELDS: [&str; 3] = ["kaish_version", "kaish_git_hash", "kaish_build_date"];

/// Run `kaish --plan <source>` and parse stdout as JSON.
fn cli_plan(source: &str) -> Value {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan")
        .arg(source)
        .output()
        .expect("run kaish --plan");
    let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
    serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout was not JSON ({e}): {stdout:?}"))
}

/// Run the in-shell `plan` builtin over the same source, through a real
/// kernel, and parse its `--json` text output as JSON.
///
/// `source` must not contain a single quote — it is spliced into a
/// single-quoted kaish argument with no escaping, same as every other test
/// in this crate that drives `plan` this way.
async fn builtin_plan(source: &str) -> (i64, Value) {
    assert!(
        !source.contains('\''),
        "test helper does not escape single quotes: {source:?}"
    );
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let cmd = format!("plan '{source}' --json");
    let r = k.execute(&cmd).await.expect("kernel execute");
    let out = r.text_out();
    let json = serde_json::from_str(&out)
        .unwrap_or_else(|e| panic!("plan --json was not JSON ({e}): {out:?}"));
    (r.code, json)
}

/// On success, both emitters print the version fields at the top level of
/// the document — and, since both read the same kaish-kernel constants, the
/// values must be identical, not merely both present.
#[tokio::test]
async fn the_cli_and_the_builtin_agree_on_version_fields_when_planning_succeeds() {
    let source = "echo hi";
    let cli = cli_plan(source);
    let (code, builtin) = builtin_plan(source).await;
    assert_eq!(code, 0, "echo hi must plan cleanly: {builtin}");

    for field in VERSION_FIELDS {
        assert!(!cli[field].is_null(), "cli plan document is missing {field}: {cli}");
        assert!(
            !builtin[field].is_null(),
            "plan builtin document is missing {field}: {builtin}"
        );
        assert_eq!(
            cli[field], builtin[field],
            "kaish --plan and the plan builtin must report the same {field} \
             for one build — they are documented to emit the same JSON"
        );
    }
}

/// The error document carries the same three fields, on both emitters —
/// about 13% of one real consumer's traffic does not plan, and they window
/// those rows by version too, so a field that vanished exactly when planning
/// fails would be the wrong shape.
///
/// The two emitters do NOT put the fields at the same JSON path here: the CLI
/// keeps `{"errors": [...], "kaish_version": ..., ...}` flat, but the
/// builtin's `--json` failure path wraps the whole document — errors and
/// version fields alike — under `data` inside the kernel's error envelope
/// (`{"error": ..., "code": 2, "data": {...}}`). That asymmetry is
/// pre-existing and documented (docs/EMBEDDING.md, "From a kaish hook body");
/// this test follows each emitter to where its fields actually live and
/// checks the VALUES still agree.
#[tokio::test]
async fn the_cli_and_the_builtin_agree_on_version_fields_when_planning_fails() {
    // Same source `plan_builtin_tests.rs` uses for its parse-error case: an
    // unterminated `for` loop, with no single quote to complicate splicing
    // it into `plan '<source>' --json`.
    let source = "for f in";

    let cli = cli_plan(source);
    assert!(
        cli["errors"].is_array(),
        "expected a CLI error document: {cli}"
    );

    let (code, builtin) = builtin_plan(source).await;
    assert_eq!(code, 2, "an unterminated for-loop is a parse error: {builtin}");
    let builtin_data = &builtin["data"];
    assert!(
        builtin_data["errors"].is_array(),
        "expected the builtin's error envelope to carry errors under data: {builtin}"
    );

    for field in VERSION_FIELDS {
        assert!(!cli[field].is_null(), "cli error document is missing {field}: {cli}");
        assert!(
            !builtin_data[field].is_null(),
            "builtin error envelope's data is missing {field}: {builtin}"
        );
        assert_eq!(
            cli[field], builtin_data[field],
            "kaish --plan and the plan builtin must report the same {field} \
             on the error path too"
        );
    }
}
