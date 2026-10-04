//! `--hashline` prints LINE:HASH:TEXT for lines read from a named file, and
//! `--json` rows carry `hash` beside `line`. Anywhere the line number is not
//! a line of one file (stdin, a concatenation, a transform), `--hashline`
//! exits 2 and names the form that works.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::LineHasher;
use std::fs;
use std::path::Path;
use tempfile::{tempdir, TempDir};

// FNV-1a, low 16 bits: alpha 202b, the empty line 2325.
fn fixture() -> TempDir {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "alpha\nbravo\ncharlie\n").unwrap();
    fs::write(dir.path().join("g.txt"), "alpha\n").unwrap();
    dir
}

fn hash(line: &str) -> String {
    kaish_types::hashline::fnv1a_line_hash(line.as_bytes())
}

async fn run(dir: &Path, script: &str) -> (i64, String, String) {
    let result = kernel_at(dir).execute(script).await.unwrap();
    (result.code, result.text_out().into_owned(), result.err)
}

async fn expect_out(dir: &Path, script: &str, expected: &str) {
    let (code, out, err) = run(dir, script).await;
    assert_eq!(code, 0, "{script}: {err}");
    assert_eq!(out, expected, "{script}");
}

async fn expect_refused(dir: &Path, script: &str, fix: &str) {
    let (code, out, err) = run(dir, script).await;
    assert_eq!(code, 2, "{script} should be refused, got out={out:?} err={err:?}");
    assert!(err.contains(fix), "{script}: error should name `{fix}`: {err}");
}

#[tokio::test]
async fn cat_prints_anchors() {
    let dir = fixture();
    let expected = format!(
        "1:202b:alpha\n2:{}:bravo\n3:{}:charlie\n",
        hash("bravo"),
        hash("charlie")
    );
    expect_out(dir.path(), "cat --hashline f.txt", &expected).await;
    expect_out(dir.path(), "cat -n --hashline f.txt", &expected).await;
}

#[tokio::test]
async fn cat_hashline_feeds_a_pipe_as_anchored_text() {
    let dir = fixture();
    let expected = format!("2:{}:bravo\n", hash("bravo"));
    expect_out(dir.path(), "cat --hashline f.txt | grep bravo", &expected).await;
}

#[tokio::test]
async fn crlf_terminators_are_not_hashed() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("w.txt"), "alpha\r\nbravo\r\n").unwrap();
    let expected = format!("1:202b:alpha\n2:{}:bravo\n", hash("bravo"));
    expect_out(dir.path(), "cat --hashline w.txt", &expected).await;
    expect_out(dir.path(), "grep --hashline a w.txt", &expected).await;
}

#[tokio::test]
async fn an_empty_file_prints_nothing() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("empty.txt"), "").unwrap();
    expect_out(dir.path(), "cat --hashline empty.txt", "").await;
}

#[tokio::test]
async fn head_and_tail_use_file_line_numbers() {
    let dir = fixture();
    expect_out(dir.path(), "head -n 1 --hashline f.txt", "1:202b:alpha\n").await;
    let expected = format!("2:{}:bravo\n3:{}:charlie\n", hash("bravo"), hash("charlie"));
    expect_out(dir.path(), "tail -n 2 --hashline f.txt", &expected).await;
    expect_out(dir.path(), "tail -n +2 --hashline f.txt", &expected).await;
}

#[tokio::test]
async fn grep_prints_anchors_with_file_names_for_several_files() {
    let dir = fixture();
    expect_out(dir.path(), "grep --hashline alpha f.txt", "1:202b:alpha\n").await;
    expect_out(
        dir.path(),
        "grep --hashline alpha f.txt g.txt",
        "f.txt:1:202b:alpha\ng.txt:1:202b:alpha\n",
    )
    .await;
}

// The anchor names the whole line even when -o prints only the match.
#[tokio::test]
async fn grep_only_matching_anchors_the_whole_line() {
    let dir = fixture();
    let expected = format!("2:{}:rav\n", hash("bravo"));
    expect_out(dir.path(), "grep -o --hashline rav f.txt", &expected).await;
}

// Context lines are file lines too; they get anchors instead of vanishing.
#[tokio::test]
async fn grep_context_lines_are_anchored() {
    let dir = fixture();
    let expected = format!("1:202b:alpha\n2:{}:bravo\n", hash("bravo"));
    expect_out(dir.path(), "grep -B 1 --hashline bravo f.txt", &expected).await;
}

#[tokio::test]
async fn json_rows_carry_the_hash() {
    let dir = fixture();
    let (code, out, err) = run(dir.path(), "head -n 1 --json f.txt").await;
    assert_eq!(code, 0, "{err}");
    let rows: serde_json::Value = serde_json::from_str(&out).unwrap();
    assert_eq!(rows, serde_json::json!([{"TEXT": "alpha", "line": 1, "hash": "202b"}]));

    let (code, out, err) = run(dir.path(), "grep --json alpha f.txt").await;
    assert_eq!(code, 0, "{err}");
    let rows: serde_json::Value = serde_json::from_str(&out).unwrap();
    assert_eq!(rows[0]["line"], 1);
    assert_eq!(rows[0]["hash"], "202b");
}

#[tokio::test]
async fn stdin_lines_have_no_anchor() {
    let dir = fixture();
    expect_refused(dir.path(), "cat f.txt | cat --hashline", "cat --hashline FILE").await;
    expect_refused(dir.path(), "cat f.txt | head --hashline", "head --hashline FILE").await;
    expect_refused(dir.path(), "cat f.txt | tail --hashline", "tail --hashline FILE").await;
    expect_refused(dir.path(), "cat f.txt | grep --hashline a", "grep --hashline PATTERN FILE")
        .await;

    // A stdin row's `line` is its place in the stream, so --json gives it no hash.
    let (code, out, err) = run(dir.path(), "cat f.txt | head -n 1 --json").await;
    assert_eq!(code, 0, "{err}");
    let rows: serde_json::Value = serde_json::from_str(&out).unwrap();
    assert_eq!(rows, serde_json::json!([{"TEXT": "alpha", "line": 1}]));
}

#[tokio::test]
async fn several_files_are_refused_where_line_numbers_would_be_ambiguous() {
    let dir = fixture();
    expect_refused(dir.path(), "cat --hashline f.txt g.txt", "one file").await;
    expect_refused(dir.path(), "head --hashline f.txt g.txt", "one file").await;
    expect_refused(dir.path(), "tail --hashline f.txt g.txt", "one file").await;
}

#[tokio::test]
async fn byte_and_marked_up_output_is_refused() {
    let dir = fixture();
    expect_refused(dir.path(), "head -c 3 --hashline f.txt", "-c").await;
    expect_refused(dir.path(), "tail -c 3 --hashline f.txt", "-c").await;
    expect_refused(dir.path(), "cat -A --hashline f.txt", "-A").await;
}

// Only the four builtins that produce anchors take the flag; the rest
// refuse it by name, and grep modes without line rows refuse the render.
#[tokio::test]
async fn output_without_anchors_is_refused() {
    let dir = fixture();
    expect_refused(dir.path(), "ls --hashline", "cat --hashline FILE").await;
    expect_refused(dir.path(), "echo hi --hashline", "cat --hashline FILE").await;
    expect_refused(dir.path(), "grep -c --hashline alpha f.txt", "cat --hashline FILE").await;
    expect_refused(dir.path(), "grep -l --hashline alpha f.txt", "cat --hashline FILE").await;
}

#[tokio::test]
async fn the_flag_is_published_only_where_anchors_exist() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());
    let schemas = kernel.tool_schemas();
    for schema in &schemas {
        let has_flag = schema.params.iter().any(|param| param.name == "hashline");
        let anchored = matches!(schema.name.as_str(), "cat" | "head" | "tail" | "grep");
        assert_eq!(has_flag, anchored, "{}: hashline param published = {has_flag}", schema.name);
    }
}

#[tokio::test]
async fn grep_without_a_match_exits_1_as_usual() {
    let dir = fixture();
    let (code, out, _) = run(dir.path(), "grep --hashline zulu f.txt").await;
    assert_eq!(code, 1);
    assert_eq!(out, "");
}

// The embedder's hasher reaches builtins in every context the kernel makes.
#[tokio::test]
async fn a_configured_hasher_is_used_everywhere() {
    let dir = fixture();
    let config = KernelConfig::repl()
        .with_cwd(dir.path().to_path_buf())
        .with_trash(false)
        .with_line_hasher(LineHasher::new(|line| format!("{:02x}", line.len())));
    let kernel = Kernel::new(config).unwrap();
    for script in [
        "cat --hashline g.txt",
        "cat --hashline g.txt | cat",
        "show() { cat --hashline g.txt; }; show",
        "x=$(cat --hashline g.txt); echo \"$x\"",
    ] {
        let result = kernel.execute(script).await.unwrap();
        assert_eq!(result.code, 0, "{script}: {}", result.err);
        assert_eq!(result.text_out(), "1:05:alpha\n", "{script}");
    }
}
