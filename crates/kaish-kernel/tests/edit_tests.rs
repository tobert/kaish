//! `edit FILE ANCHOR TEXT ...` changes lines by the anchors `--hashline`
//! prints. Every anchor in one call refers to the file before the call, and
//! nothing is written unless every anchor matches.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;
use std::path::Path;
use tempfile::{tempdir, TempDir};

const CONFIG: &str = "[server]\nhost = \"127.0.0.1\"\nport = 8080\ntimeout = 30\n\n[logging]\nlevel = \"info\"\nfile = \"/var/log/app.log\"\nrotate = true\n\n[cache]\nenabled = false\nsize = 256\n";

fn hash(line: &str) -> String {
    kaish_types::hashline::fnv1a_line_hash(line.as_bytes())
}

fn anchor(line: usize, text: &str) -> String {
    format!("{line}:{}", hash(text))
}

fn fixture() -> TempDir {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("config.toml"), CONFIG).unwrap();
    dir
}

fn read(dir: &Path) -> String {
    fs::read_to_string(dir.join("config.toml")).unwrap()
}

async fn run(dir: &Path, script: &str) -> (i64, String, String) {
    let result = kernel_at(dir).execute(script).await.unwrap();
    (result.code, result.text_out().into_owned(), result.err)
}

async fn run_ok(dir: &Path, script: &str) -> String {
    let (code, out, err) = run(dir, script).await;
    assert_eq!(code, 0, "{script}: {err}");
    out
}

#[tokio::test]
async fn replace_one_line_and_print_its_new_anchor() {
    let dir = fixture();
    let out = run_ok(dir.path(), &format!("edit config.toml {} 'port = 9090'", anchor(3, "port = 8080"))).await;
    assert_eq!(read(dir.path()), CONFIG.replace("port = 8080", "port = 9090"));
    assert_eq!(out, format!("3:{}:port = 9090\n", hash("port = 9090")));
}

#[tokio::test]
async fn delete_a_range() {
    let dir = fixture();
    let script = format!("edit config.toml --delete {}..{}", anchor(6, "[logging]"), anchor(10, ""));
    let out = run_ok(dir.path(), &script).await;
    assert_eq!(
        read(dir.path()),
        "[server]\nhost = \"127.0.0.1\"\nport = 8080\ntimeout = 30\n\n[cache]\nenabled = false\nsize = 256\n"
    );
    assert_eq!(out, "", "a deletion leaves no new lines to show");
}

#[tokio::test]
async fn insert_after_and_before() {
    let dir = fixture();
    let host = anchor(2, "host = \"127.0.0.1\"");
    let first = anchor(1, "[server]");
    let out = run_ok(
        dir.path(),
        &format!("edit config.toml --after {host} 'workers = 4' --before {first} '# app'"),
    )
    .await;
    assert!(read(dir.path()).starts_with("# app\n[server]\nhost = \"127.0.0.1\"\nworkers = 4\nport = 8080\n"));
    assert_eq!(
        out,
        format!("1:{}:# app\n4:{}:workers = 4\n", hash("# app"), hash("workers = 4"))
    );
}

#[tokio::test]
async fn a_batch_resolves_every_anchor_against_the_original_file() {
    let dir = fixture();
    // The first edit grows line 3 into two lines; the later anchors still
    // name lines of the file as it was read.
    let script = format!(
        "edit config.toml {} 'port = 9090\nthreads = 8' {} 'enabled = true' {} 'size = 512' {} 'level = \"debug\"'",
        anchor(3, "port = 8080"),
        anchor(12, "enabled = false"),
        anchor(13, "size = 256"),
        anchor(7, "level = \"info\""),
    );
    run_ok(dir.path(), &script).await;
    let expected = CONFIG
        .replace("port = 8080", "port = 9090\nthreads = 8")
        .replace("enabled = false", "enabled = true")
        .replace("size = 256", "size = 512")
        .replace("level = \"info\"", "level = \"debug\"");
    assert_eq!(read(dir.path()), expected);
}

#[tokio::test]
async fn append_at_the_end() {
    let dir = fixture();
    let last = anchor(13, "size = 256");
    run_ok(dir.path(), &format!("edit config.toml --after {last} 'ttl = 60\nbackend = \"memory\"'")).await;
    assert!(read(dir.path()).ends_with("size = 256\nttl = 60\nbackend = \"memory\"\n"));
}

#[tokio::test]
async fn a_stale_anchor_writes_nothing_and_asks_for_a_reread() {
    let dir = fixture();
    let (code, out, err) = run(dir.path(), "edit config.toml 12:0000 'enabled = true'").await;
    assert_eq!(code, 1);
    assert_eq!(out, "");
    assert_eq!(read(dir.path()), CONFIG);
    assert!(err.contains("line 12 changed since you read it (now: enabled = false)"), "{err}");
    assert!(err.contains("nothing was written"), "{err}");
    assert!(err.contains("cat --hashline config.toml"), "{err}");
    // No ready-made anchor to retry with: the model must read again.
    assert!(!err.contains(&anchor(12, "enabled = false")), "{err}");
}

#[tokio::test]
async fn one_stale_anchor_in_a_batch_stops_the_whole_batch() {
    let dir = fixture();
    let script = format!("edit config.toml {} 'port = 9090' 40:abcd 'x'", anchor(3, "port = 8080"));
    let (code, _, err) = run(dir.path(), &script).await;
    assert_eq!(code, 1);
    assert_eq!(read(dir.path()), CONFIG);
    assert!(err.contains("line 40 is past the end (config.toml has 13 lines)"), "{err}");
}

#[tokio::test]
async fn usage_errors_name_the_fix() {
    let dir = fixture();
    for (script, fix) in [
        ("edit", "edit FILE ANCHOR TEXT"),
        ("edit config.toml", "edit FILE ANCHOR TEXT"),
        ("edit config.toml 3a8c7 'x'", "LINE:HASH"),
        (&*format!("edit config.toml {}", anchor(3, "port = 8080")), "--delete"),
        ("edit config.toml --after 1:63d0..2:7f4b 'x'", "one anchor"),
        ("edit config.toml --frobnicate", "--frobnicate"),
    ] {
        let (code, _, err) = run(dir.path(), script).await;
        assert_eq!(code, 2, "{script}: {err}");
        assert!(err.contains(fix), "{script}: error should name `{fix}`: {err}");
    }
    assert_eq!(read(dir.path()), CONFIG);
}

#[tokio::test]
async fn overlapping_edits_are_refused() {
    let dir = fixture();
    let range = format!("{}..{}", anchor(6, "[logging]"), anchor(10, ""));
    let level = anchor(7, "level = \"info\"");
    for script in [
        format!("edit config.toml --delete {range} {level} 'level = 1'"),
        format!("edit config.toml --delete {range} --after {level} 'x = 1'"),
        format!("edit config.toml {level} 'a' {level} 'b'"),
    ] {
        let (code, _, err) = run(dir.path(), &script).await;
        assert_eq!(code, 2, "{script}: {err}");
        assert!(err.contains("overlap") || err.contains("inside"), "{script}: {err}");
    }
    assert_eq!(read(dir.path()), CONFIG);
}

// Text that looks like a flag is still text: it follows an anchor.
#[tokio::test]
async fn text_may_start_with_a_dash() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("notes.md"), "- one\n- two\n").unwrap();
    let (code, _, err) = run(dir.path(), &format!("edit notes.md {} '--two'", anchor(2, "- two"))).await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(fs::read_to_string(dir.path().join("notes.md")).unwrap(), "- one\n--two\n");
}

#[tokio::test]
async fn crlf_files_stay_crlf() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("w.txt"), "alpha\r\nbravo\r\n").unwrap();
    run_ok(dir.path(), &format!("edit w.txt {} 'one\ntwo'", anchor(1, "alpha"))).await;
    assert_eq!(fs::read_to_string(dir.path().join("w.txt")).unwrap(), "one\r\ntwo\r\nbravo\r\n");
}

#[tokio::test]
async fn a_missing_final_newline_stays_missing() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "alpha\nbravo").unwrap();
    run_ok(dir.path(), &format!("edit f.txt {} 'BRAVO'", anchor(2, "bravo"))).await;
    assert_eq!(fs::read_to_string(dir.path().join("f.txt")).unwrap(), "alpha\nBRAVO");
}

#[tokio::test]
async fn quiet_prints_nothing_and_json_carries_anchors() {
    let dir = fixture();
    let out = run_ok(dir.path(), &format!("edit -q config.toml {} 'port = 1'", anchor(3, "port = 8080"))).await;
    assert_eq!(out, "");
    let out = run_ok(dir.path(), &format!("edit config.toml {} 'port = 2' --json", anchor(3, "port = 1"))).await;
    let rows: serde_json::Value = serde_json::from_str(&out).unwrap();
    assert_eq!(rows, serde_json::json!([{"TEXT": "port = 2", "line": 3, "hash": hash("port = 2")}]));
}

#[tokio::test]
async fn a_large_change_prints_a_summary_instead() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "a\nb\n").unwrap();
    let body: Vec<String> = (1..=50).map(|n| format!("line {n}")).collect();
    let script = format!("edit f.txt {} '{}'", anchor(1, "a"), body.join("\n"));
    let out = run_ok(dir.path(), &script).await;
    assert_eq!(out.lines().count(), 1, "{out}");
    assert!(out.contains("50 lines"), "{out}");
    assert!(out.contains("tail -n +1 --hashline f.txt | head -n 50"), "{out}");
}

// The anchors edit prints are the ones cat --hashline prints next.
#[tokio::test]
async fn printed_anchors_feed_the_next_edit() {
    let dir = fixture();
    let out = run_ok(dir.path(), &format!("edit config.toml {} 'port = 9090'", anchor(3, "port = 8080"))).await;
    let new_anchor = out.split(':').take(2).collect::<Vec<_>>().join(":");
    run_ok(dir.path(), &format!("edit config.toml {new_anchor} 'port = 9191'")).await;
    assert!(read(dir.path()).contains("port = 9191\n"));
}

#[tokio::test]
async fn a_missing_or_binary_file_fails_without_writing() {
    let dir = tempdir().unwrap();
    let (code, _, err) = run(dir.path(), "edit nope.txt 1:202b 'x'").await;
    assert_eq!(code, 1, "{err}");
    fs::write(dir.path().join("bin"), b"\xff\xfe\n").unwrap();
    let (code, _, err) = run(dir.path(), "edit bin 1:202b 'x'").await;
    assert_eq!(code, 1, "{err}");
    assert!(err.contains("UTF-8"), "{err}");
    assert_eq!(fs::read(dir.path().join("bin")).unwrap(), b"\xff\xfe\n");
}

#[cfg(unix)]
#[tokio::test]
async fn edit_replaces_the_file_atomically() {
    use std::os::unix::fs::MetadataExt;
    let dir = fixture();
    let path = dir.path().join("config.toml");
    let before = fs::metadata(&path).unwrap().ino();
    run_ok(dir.path(), &format!("edit config.toml {} 'port = 9090'", anchor(3, "port = 8080"))).await;
    assert_ne!(fs::metadata(&path).unwrap().ino(), before);
}
