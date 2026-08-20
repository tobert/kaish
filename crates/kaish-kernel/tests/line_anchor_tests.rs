//! One line anchor, one name, one type, across every builtin that has one.
//!
//! "Which line of the file is this" had three representations under `--json`:
//!
//! - `grep` — an integer under `line_number`, in its `rich_json` override,
//!   emitted whether or not `-n` was given.
//! - `head`/`tail` — a **string** under `NUM`, and a `LINE` column holding the
//!   line's *text*. `grep`'s `LINE` column held the *number*, so one header
//!   name meant both things depending on the builtin.
//! - `cat -n` — nothing structured at all; the number was formatted into the
//!   text with `%6d\t`.
//!
//! Nothing above the builtin could read a line number without knowing which
//! builtin produced it. `OutputNode::at_line` makes it a typed field the
//! builtin declares and every consumer reads, and `--json` renders it as an
//! integer named `line`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// The fixtures write real files.
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::{kernel_at, run};
use serde_json::Value as Json;
use tempfile::TempDir;

/// Five lines, so a `tail` anchor is provably a file line number rather than
/// an index into the rows the builtin happened to emit.
fn fixture() -> TempDir {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "alpha\nbravo\nmatch me\ndelta\nmatch me too\n").unwrap();
    dir
}

async fn json_rows(script: &str) -> Vec<Json> {
    let dir = fixture();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, script).await;
    assert_eq!(code, 0, "`{script}` exited {code}: {out}");
    match serde_json::from_str::<Json>(&out) {
        Ok(Json::Array(rows)) => rows,
        Ok(other) => panic!("`{script}` produced {other} — expected an array of rows"),
        Err(e) => panic!("`{script}` produced unparseable JSON ({e}): {out}"),
    }
}

/// Every row's `line`, insisting it is a JSON integer and not a string.
fn anchors(rows: &[Json]) -> Vec<u64> {
    rows.iter()
        .map(|row| {
            let line = row
                .get("line")
                .unwrap_or_else(|| panic!("row has no `line` anchor: {row}"));
            line.as_u64()
                .unwrap_or_else(|| panic!("`line` is {line}, not an integer, in {row}"))
        })
        .collect()
}

#[tokio::test]
async fn head_anchors_are_file_line_numbers() {
    let rows = json_rows("head -n 3 f.txt --json").await;
    assert_eq!(anchors(&rows), vec![1, 2, 3]);
}

/// `tail`'s anchor is the line's position in the FILE, not its position among
/// the rows tail emitted — the distinction a stringly `NUM` column made easy
/// to get wrong silently.
#[tokio::test]
async fn tail_anchors_are_file_line_numbers() {
    let rows = json_rows("tail -n 2 f.txt --json").await;
    assert_eq!(anchors(&rows), vec![4, 5]);
}

#[tokio::test]
async fn grep_anchors_are_file_line_numbers() {
    let rows = json_rows("grep -n match f.txt --json").await;
    assert_eq!(anchors(&rows), vec![3, 5]);
}

/// `grep` numbers unconditionally — `-n` controls the text rendering, never
/// whether the anchor exists. This already held for the `rich_json` payload
/// and now holds under the shared name.
#[tokio::test]
async fn grep_anchors_survive_without_dash_n() {
    let rows = json_rows("grep match f.txt --json").await;
    assert_eq!(anchors(&rows), vec![3, 5]);
}

/// `cat -n` asks for line numbers by name. Without `--json` it still prints
/// the GNU `%6d\t` text; with it, the number is data.
#[tokio::test]
async fn cat_dash_n_anchors_are_file_line_numbers() {
    let rows = json_rows("cat -n f.txt --json").await;
    assert_eq!(anchors(&rows), vec![1, 2, 3, 4, 5]);
}

/// Plain `cat` stays byte-clean: no rows, no anchor, just the content as a
/// JSON string. Adding an anchor here would mean chopping the stream into
/// lines, which is exactly what `cat` must not do.
#[tokio::test]
async fn plain_cat_stays_a_json_string() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat f.txt --json").await;
    assert_eq!(code, 0);
    let parsed: Json = serde_json::from_str(&out).expect("cat --json parses");
    assert!(parsed.is_string(), "cat --json should stay a string, got {parsed}");
}

/// A builtin with no line anchor must not grow a null one — the field is
/// absent, not present-and-empty, so `jq 'has("line")'` is a real question.
#[tokio::test]
async fn rows_without_an_anchor_have_no_line_key() {
    let rows = json_rows("ls --json").await;
    let row = rows.first().expect("ls listed something");
    assert!(
        !row.as_object().map(|o| o.contains_key("line")).unwrap_or(false),
        "ls row should carry no `line` key: {row}"
    );
}

/// The text of the line and its number are different things and no longer
/// share a header name. `head`'s text column was `LINE` while `grep`'s `LINE`
/// column was the number.
#[tokio::test]
async fn head_row_carries_the_text_under_its_own_key() {
    let rows = json_rows("head -n 1 f.txt --json").await;
    let row = &rows[0];
    assert_eq!(row.get("TEXT").and_then(Json::as_str), Some("alpha"), "{row}");
    assert!(row.get("LINE").is_none(), "`LINE` must not name the text: {row}");
    assert!(row.get("NUM").is_none(), "`NUM` is replaced by `line`: {row}");
}

/// `cat -n f` and `… | cat -n` number the same lines, so they answer `--json`
/// the same way. The stdin path formatted `%6d\t` into text and stopped there,
/// which made the anchor reachable through one spelling and not the other.
#[tokio::test]
async fn cat_dash_n_anchors_match_through_a_pipe() {
    let rows = json_rows("cat f.txt | cat -n --json").await;
    assert_eq!(anchors(&rows), vec![1, 2, 3, 4, 5]);
}

/// Without `--json`, `cat -n` still prints GNU's `%6d\t` text on both paths.
#[tokio::test]
async fn cat_dash_n_text_is_unchanged_on_both_paths() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());
    let (from_file, _) = run(&kernel, "cat -n f.txt").await;
    let (from_pipe, _) = run(&kernel, "cat f.txt | cat -n").await;
    // `run` trims, so the first line's leading pad is gone; the second row
    // still carries GNU's six-column pad.
    assert!(from_file.starts_with("1\talpha\n     2\tbravo"), "{from_file:?}");
    assert_eq!(from_file, from_pipe);
}
