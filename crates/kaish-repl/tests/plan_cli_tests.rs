//! `kaish --plan`: the analysis surface as JSON, for consumers that are not
//! written in Rust.
//!
//! Everything below drives the real binary, because the contract being tested
//! is the process one — argv in, JSON on stdout, an exit code that says which
//! shape it is. A library-level test of `plan_program` would pass while the
//! CLI printed nothing.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::process::Command;

use serde_json::Value;

/// Run `kaish --plan <source>` and return its exit code and parsed stdout.
///
/// The output is parsed as JSON unconditionally: a caller of this CLI parses
/// one shape whatever happened, and a test that only parsed the success case
/// would not notice the error path emitting prose.
fn plan(source: &str) -> (i32, Value) {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan")
        .arg(source)
        .output()
        .expect("run kaish --plan");
    let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
    let json = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout was not JSON ({e}): {stdout:?}"));
    (out.status.code().expect("exit code"), json)
}

/// Every heredoc in the first statement of a planned source.
fn heredocs(json: &Value) -> Vec<Value> {
    json["statements"]
        .as_array()
        .expect("statements array")
        .iter()
        .flat_map(|s| s["plan"]["commands"].as_array().expect("commands").iter())
        .flat_map(|c| {
            c["heredocs"]
                .as_array()
                .map(|a| a.to_vec())
                .unwrap_or_default()
        })
        .collect()
}

#[test]
fn planning_a_heredoc_emits_the_command_delimiter_and_body() {
    let (code, json) = plan("python3 <<'PY'\nimport os\nPY");
    assert_eq!(code, 0);

    let command = &json["statements"][0]["plan"]["commands"][0];
    assert_eq!(command["name"], "python3");

    let heredoc = &heredocs(&json)[0];
    assert_eq!(heredoc["delimiter"], "PY");
    assert_eq!(heredoc["literal"], true);
    assert_eq!(heredoc["body"]["plain"], "import os\n");
}

/// The property the exclusion use case rests on: `body_offset` and the body's
/// length slice the body back out of the source exactly, so a consumer can
/// classify the shell framing and leave the payload out of it.
///
/// This is why the published body is verbatim source rather than the parsed
/// form — the lexer rewrites `$((…))` inside an interpolated body into a
/// longer internal spelling, and publishing that would shift every offset
/// after it. The arithmetic case below is here to catch exactly that.
#[test]
fn the_body_offset_slices_the_body_out_of_the_source() {
    for source in [
        "python3 <<'PY'\nimport os\nPY",
        "cat x | python3 <<'PY'\nprint(1)\nPY",
        "for f in a b; do python3 <<-'PY'\n\tindented\n\tPY\ndone",
        "python3 <<PY\nn = $((1 + 2))\nx = ${NAME}\nPY",
        "echo hi\n# a comment\nsqlite3 db <<SQL\nselect 1;\nSQL",
    ] {
        let (code, json) = plan(source);
        assert_eq!(code, 0, "source: {source}");
        for heredoc in heredocs(&json) {
            let body = heredoc["body"]["plain"].as_str().expect("body text");
            let start = heredoc["body_offset"].as_u64().expect("offset") as usize;
            assert_eq!(
                source.get(start..start + body.len()),
                Some(body),
                "offset {start} does not slice the body in: {source}",
            );
        }
    }
}

/// A parse failure is still JSON, and it exits 2 — the usage code a builtin
/// returns for bad argv. A consumer that got prose on stderr and an empty
/// stdout would have to special-case the failure path.
#[test]
fn a_parse_error_is_json_and_exits_2() {
    let (code, json) = plan("python3 <<'PY'\nunterminated");
    assert_eq!(code, 2);
    assert!(json["statements"].is_null(), "errors must not carry statements");
    let message = json["errors"][0]["message"].as_str().expect("message");
    assert!(
        message.contains("unterminated heredoc"),
        "message should name the failure: {message}"
    );
    assert!(json["errors"][0]["start"].is_number());
}

/// Planning runs nothing. The statement below would delete a file; after
/// planning it, the file is still there.
#[test]
fn planning_executes_nothing() {
    let dir = tempfile::Builder::new()
        .prefix("plan-cli-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir");
    let target = dir.path().join("precious.txt");
    std::fs::write(&target, "keep me").expect("write");

    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .current_dir(dir.path())
        .arg("--plan")
        .arg("rm precious.txt")
        .output()
        .expect("run kaish --plan");

    assert_eq!(out.status.code(), Some(0));
    assert!(target.exists(), "planning must not run the statement");
    let json: Value = serde_json::from_slice(&out.stdout).expect("json");
    assert_eq!(json["statements"][0]["plan"]["commands"][0]["name"], "rm");
}

/// The statement index is the position in the *parsed* program, counted
/// before empty statements are dropped — so a leading comment leaves a gap.
/// A consumer that filtered and then indexed by position would read the wrong
/// statement, which is exactly what it did inside the kernel until review
/// caught it.
#[test]
fn the_statement_index_counts_empty_statements() {
    let (code, json) = plan("# a comment\necho one\necho two");
    assert_eq!(code, 0);
    let indices: Vec<u64> = json["statements"]
        .as_array()
        .expect("statements")
        .iter()
        .map(|s| s["index"].as_u64().expect("index"))
        .collect();
    assert_eq!(indices, vec![1, 2], "the comment holds index 0");
}

/// `--plan` with no source names what it wants rather than planning an empty
/// program and reporting success.
#[test]
fn plan_without_a_source_is_an_error() {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan")
        .output()
        .expect("run kaish --plan");
    assert_ne!(out.status.code(), Some(0));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--plan requires a command argument"),
        "stderr should name the missing argument: {stderr}"
    );
}
