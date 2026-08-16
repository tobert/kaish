//! `kaish --plan`: command analysis as JSON, for consumers that are not
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
        // Multibyte before AND inside the body: `body_offset` is a byte
        // offset and `body.len()` is a byte length, so a char-counting
        // mistake anywhere would tear the slice. The first line is quoted
        // because every bareword rule in the lexer uses ASCII-only character
        // classes, so an unquoted `日本語` or `café` is a lexer error.
        "echo \"日本語\"\npython3 <<'PY'\nprint(\"こんにちは\")\nPY",
        // CRLF: the terminator's bytes are part of the body.
        "python3 <<'PY'\r\nimport os\r\nPY",
        // Reached only through a command substitution.
        "out=$(cat <<'A'\nnested\nA\n)",
        // Two heredocs: the first must not shift the second's offset.
        "python3 <<'A' | python3 <<'B'\nfirst\nA\nsecond\nB",
        // A body that is empty, and one that is only a newline.
        "python3 <<'PY'\nPY",
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

/// `--plan` with no source is reported through the same door as a broken
/// source: JSON on stdout, exit 2. "Always a JSON object, except when you
/// called it wrong" is the case a caller would not have written a branch for,
/// so there is no exception.
#[test]
fn plan_without_a_source_is_json_and_exits_2() {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan")
        .output()
        .expect("run kaish --plan");
    assert_eq!(out.status.code(), Some(2));

    let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
    let json: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout was not JSON ({e}): {stdout:?}"));
    let message = json["errors"][0]["message"].as_str().expect("message");
    assert!(
        message.contains("--plan requires a command argument"),
        "the error should name what is missing: {message}"
    );
}

/// The contract has no third outcome: every way of calling `--plan` prints a
/// JSON object and exits 0 or 2. A caller writes one parse and two branches.
#[test]
fn every_outcome_is_json_with_exit_0_or_2() {
    for (args, expected) in [
        (vec!["--plan", "echo hi"], 0),
        (vec!["--plan", ""], 0),
        (vec!["--plan", "python3 <<'PY'\nunterminated"], 2),
        (vec!["--plan"], 2),
        // `--overlay` is filtered out wherever it sits; the source still plans.
        (vec!["--plan", "--overlay", "echo hi"], 0),
    ] {
        let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
            .args(&args)
            .output()
            .expect("run kaish");
        let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
        assert_eq!(out.status.code(), Some(expected), "args: {args:?}");
        let json: Value = serde_json::from_str(&stdout)
            .unwrap_or_else(|e| panic!("not JSON for {args:?} ({e}): {stdout:?}"));
        assert!(json.is_object(), "not an object for {args:?}: {json}");
        // The shape and the code agree, always.
        assert_eq!(
            json.get("statements").is_some(),
            expected == 0,
            "shape disagrees with exit {expected} for {args:?}: {json}"
        );
    }
}

// ───────────────── Reading the source from a file ──────────────────

/// A whole script does not fit comfortably in argv, and a caller measuring
/// real traffic should not have to shell-quote it to ask a question about it.
/// `--plan-file` reads the same source from a path and answers identically.
#[test]
fn planning_a_file_matches_planning_the_same_source_inline() {
    let source = "python3 <<'PY'\nimport os\nPY";
    let dir = tempfile::Builder::new()
        .prefix("plan-file-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir");
    let path = dir.path().join("demo.kai");
    std::fs::write(&path, source).expect("write");

    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan-file")
        .arg(&path)
        .output()
        .expect("run kaish --plan-file");
    assert_eq!(out.status.code(), Some(0));
    let from_file: Value = serde_json::from_slice(&out.stdout).expect("json");

    let (code, inline) = plan(source);
    assert_eq!(code, 0);
    assert_eq!(from_file, inline, "a file and an argument must plan alike");
}

/// A path that cannot be read is reported through the same door as a broken
/// source: JSON, exit 2. It names the path and the reason, because "could not
/// read" alone leaves the caller guessing between absent and unreadable.
#[test]
fn an_unreadable_path_is_json_and_exits_2() {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan-file")
        .arg("/nonexistent/nowhere.kai")
        .output()
        .expect("run kaish --plan-file");
    assert_eq!(out.status.code(), Some(2));

    let json: Value = serde_json::from_slice(&out.stdout).expect("json");
    let message = json["errors"][0]["message"].as_str().expect("message");
    assert!(
        message.contains("/nonexistent/nowhere.kai"),
        "the error should name the path: {message}"
    );
}

/// `--plan-file` with no path names the form it wants, including the `-`
/// spelling, rather than reading stdin by accident.
#[test]
fn plan_file_without_a_path_is_json_and_exits_2() {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan-file")
        .output()
        .expect("run kaish --plan-file");
    assert_eq!(out.status.code(), Some(2));
    let json: Value = serde_json::from_slice(&out.stdout).expect("json");
    let message = json["errors"][0]["message"].as_str().expect("message");
    assert!(message.contains("--plan-file <path>"), "got: {message}");
    assert!(message.contains("- for stdin"), "got: {message}");
}

/// A heredoc whose introducer line carries more command after it — `cat
/// <<'EOF' && echo done` — starts its body after that whole line, not after
/// the delimiter word. A scanner that starts at the delimiter swallows the
/// `&& …` tail into the body, which for a guard means hiding real command
/// text inside what it excluded as data.
///
/// Found by diffing these spans against a hand-rolled regex scanner on real
/// traffic: 16 of 249 heredocs disagreed, every one this shape, every one in
/// that direction.
#[test]
fn a_heredoc_body_starts_after_the_whole_introducer_line() {
    for (source, expected) in [
        ("cat <<'EOF' && echo done\nmessage\nEOF", "message\n"),
        (
            "git commit -F - <<'EOF' && git push origin main\na message\nhere\nEOF",
            "a message\nhere\n",
        ),
        ("cat <<'EOF' | tee out.txt && echo ok\nbody\nEOF", "body\n"),
    ] {
        let (code, json) = plan(source);
        assert_eq!(code, 0, "source: {source}");
        let heredoc = &heredocs(&json)[0];
        assert_eq!(heredoc["body"]["plain"], expected, "source: {source}");
        // The whole introducer line, `&&` tail included, sits before the body.
        let start = heredoc["body_offset"].as_u64().expect("offset") as usize;
        assert!(
            source[..start].ends_with('\n') && source[..start].contains("&&"),
            "the body must start after the introducer line: {:?}",
            &source[..start]
        );
    }
}
