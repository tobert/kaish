//! Kernel-routed coverage for builtins that take multiple positional paths,
//! globs, or stdin — the contract surface the old direct-`.execute()` unit
//! tests bypass (see `ls_tests.rs` for the bug that bypass hid).
//!
//! The 2026-05-28 multi-positional sweep fixed cp/mv/cut/sort/stat/dirname/
//! realpath/readlink/cat/wc to read every positional, but those fixes had no
//! kernel-routed test locking them in. These do: real command strings through
//! `kernel.execute()` over a `tempfile::tempdir()` root.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

fn touch(dir: &std::path::Path, name: &str, contents: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, contents).expect("write file");
}

// --- cat: multiple files and globs ----------------------------------------

#[tokio::test]
async fn cat_concatenates_multiple_files_in_order() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "alpha\n");
    touch(dir.path(), "b.txt", "beta\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat a.txt b.txt").await;
    assert_eq!(code, 0, "cat of two files should succeed: {out:?}");
    let a = out.find("alpha").expect("alpha present");
    let b = out.find("beta").expect("beta present");
    assert!(a < b, "cat should preserve argument order: {out:?}");
}

#[tokio::test]
async fn cat_concatenates_bytes_verbatim_no_inserted_newline() {
    // cat must concatenate raw bytes — never synthesize a separator. A file
    // that ends in a newline must not gain a blank line after it.
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "alpha\nbanana\n");
    touch(dir.path(), "b.txt", "beta\n");
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat a.txt b.txt").await.expect("execute");
    assert_eq!(result.code, 0);
    assert_eq!(
        result.text_out(),
        "alpha\nbanana\nbeta\n",
        "cat inserted a spurious separator between newline-terminated files"
    );
}

#[tokio::test]
async fn cat_joins_file_without_trailing_newline() {
    // GNU cat of `x` (no trailing newline) then `y\n` yields `xy\n`, not
    // `x\ny\n`. Inserting a newline corrupts the byte stream.
    let dir = tempdir().unwrap();
    touch(dir.path(), "c.txt", "x");
    touch(dir.path(), "d.txt", "y\n");
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat c.txt d.txt").await.expect("execute");
    assert_eq!(result.code, 0);
    assert_eq!(
        result.text_out(),
        "xy\n",
        "cat inserted a newline after a file that lacked a trailing one"
    );
}

#[tokio::test]
async fn cat_glob_concatenates_all_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.log", "first\n");
    touch(dir.path(), "two.log", "second\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat *.log").await;
    assert_eq!(code, 0);
    assert!(out.contains("first"), "missing first: {out:?}");
    assert!(out.contains("second"), "missing second: {out:?}");
}

// --- wc: multiple files yield per-file rows + a total ----------------------

#[tokio::test]
async fn wc_multi_file_reports_each_and_total() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "f1.txt", "a\nb\n");
    touch(dir.path(), "f2.txt", "c\nd\ne\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "wc -l f1.txt f2.txt").await;
    assert_eq!(code, 0, "wc of two files should succeed: {out:?}");
    assert!(out.contains("f1.txt"), "missing f1 row: {out:?}");
    assert!(out.contains("f2.txt"), "missing f2 row: {out:?}");
    // 2 + 3 = 5 total lines; the total row must be present (the bug the
    // realworld suite found was multi-file wc dropping all but one row).
    assert!(out.contains('5'), "missing total of 5 lines: {out:?}");
}

// --- stat: single file produces a populated table --------------------------

#[tokio::test]
async fn stat_reports_size_for_file() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.bin", "0123456789"); // 10 bytes
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "stat data.bin").await;
    assert_eq!(code, 0, "stat should succeed: {out:?}");
    assert!(out.contains("data.bin"), "missing file name: {out:?}");
    assert!(out.contains("10"), "missing size 10: {out:?}");
}

// --- cp / mv: copy, multi-source-to-dir, rename ----------------------------

#[tokio::test]
async fn cp_copies_file_contents() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "src.txt", "payload");
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "cp src.txt dst.txt").await;
    assert_eq!(code, 0);
    let copied = fs::read_to_string(dir.path().join("dst.txt")).expect("dst exists");
    assert_eq!(copied, "payload", "copy should preserve contents");
}

#[tokio::test]
async fn cp_multiple_sources_into_directory() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "A");
    touch(dir.path(), "b.txt", "B");
    fs::create_dir(dir.path().join("dest")).unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cp a.txt b.txt dest").await;
    assert_eq!(code, 0, "multi-source cp should succeed: {out:?}");
    assert!(dir.path().join("dest/a.txt").exists(), "a.txt not copied");
    assert!(dir.path().join("dest/b.txt").exists(), "b.txt not copied");
}

#[tokio::test]
async fn cp_multiple_sources_into_directory_trailing_slash() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "A");
    touch(dir.path(), "b.txt", "B");
    fs::create_dir(dir.path().join("dest")).unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cp a.txt b.txt dest/").await;
    assert_eq!(code, 0, "multi-source cp to DST/ should succeed: {out:?}");
    assert!(dir.path().join("dest/a.txt").exists(), "a.txt not copied");
    assert!(dir.path().join("dest/b.txt").exists(), "b.txt not copied");
}

#[tokio::test]
async fn mv_renames_file() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "old.txt", "stuff");
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "mv old.txt new.txt").await;
    assert_eq!(code, 0);
    assert!(!dir.path().join("old.txt").exists(), "old name should be gone");
    assert!(dir.path().join("new.txt").exists(), "new name should exist");
}

// --- path-arithmetic builtins ----------------------------------------------

#[tokio::test]
async fn dirname_and_basename_split_path() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (dn, dc) = run(&kernel, "dirname /x/y/z.txt").await;
    assert_eq!(dc, 0);
    assert_eq!(dn, "/x/y", "dirname mismatch: {dn:?}");
    let (bn, bc) = run(&kernel, "basename /x/y/z.txt").await;
    assert_eq!(bc, 0);
    assert_eq!(bn, "z.txt", "basename mismatch: {bn:?}");
}

// --- text filters over real files ------------------------------------------

#[tokio::test]
async fn sort_orders_lines() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "lines.txt", "charlie\nalpha\nbravo\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort lines.txt").await;
    assert_eq!(code, 0);
    let a = out.find("alpha").expect("alpha");
    let b = out.find("bravo").expect("bravo");
    let c = out.find("charlie").expect("charlie");
    assert!(a < b && b < c, "sort order wrong: {out:?}");
}

#[tokio::test]
async fn cut_selects_field() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "csv.txt", "one,two,three\n");
    let kernel = kernel_at(dir.path());
    // The delimiter must be quoted: a bare `,` (`cut -d ,` or `cut -d,`) is a
    // parse error in kaish — see docs/issues.md (bash-porting ergonomics gap).
    let (out, code) = run(&kernel, "cut -d ',' -f 2 csv.txt").await;
    assert_eq!(code, 0, "cut should succeed: {out:?}");
    assert_eq!(out, "two", "cut should select the second field: {out:?}");
}

#[tokio::test]
async fn head_and_tail_bound_lines() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "five.txt", "l1\nl2\nl3\nl4\nl5\n");
    let kernel = kernel_at(dir.path());
    let (head, hc) = run(&kernel, "head -n 2 five.txt").await;
    assert_eq!(hc, 0);
    assert!(head.contains("l1") && head.contains("l2"), "head: {head:?}");
    assert!(!head.contains("l5"), "head leaked last line: {head:?}");
    let (tail, tc) = run(&kernel, "tail -n 2 five.txt").await;
    assert_eq!(tc, 0);
    assert!(tail.contains("l4") && tail.contains("l5"), "tail: {tail:?}");
    assert!(!tail.contains("l1"), "tail leaked first line: {tail:?}");
}

#[tokio::test]
async fn tr_translates_from_pipeline() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo hello | tr l L").await;
    assert_eq!(code, 0, "tr should succeed: {out:?}");
    assert_eq!(out, "heLLo", "tr translation wrong: {out:?}");
}

// ===========================================================================
// Batch 2 — path-walking / FS-mutation / multi-file-text builtins, plus a
// regression for the bare-`.` argument bug (a `.` arg was parsed as the
// `source` builtin, breaking `find .`, `ls .`, `echo .`, etc.).
// ===========================================================================

#[tokio::test]
async fn dot_argument_is_literal_not_source() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo .").await;
    assert_eq!(code, 0, "echo . should succeed: {out:?}");
    assert_eq!(out, ".", "bare . arg should be the literal dot: {out:?}");
}

#[tokio::test]
async fn ls_dot_lists_current_directory() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "here.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls .").await;
    assert_eq!(code, 0, "ls . should succeed: {out:?}");
    assert!(out.contains("here.txt"), "ls . missed entry: {out:?}");
}

#[tokio::test]
async fn source_alias_still_works_in_command_position() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "script.kai", "echo from-source\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, ". script.kai").await;
    assert_eq!(code, 0, ". (source) should succeed: {out:?}");
    assert!(out.contains("from-source"), "source didn't run script: {out:?}");
}

#[tokio::test]
async fn find_name_filters_to_matches_recursively() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "keep.log", "");
    touch(dir.path(), "skip.txt", "");
    touch(dir.path(), "sub/nested.log", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -name '*.log'").await;
    assert_eq!(code, 0, "find should succeed: {out:?}");
    assert!(out.contains("keep.log"), "find missed keep.log: {out:?}");
    assert!(out.contains("nested.log"), "find missed nested.log: {out:?}");
    assert!(!out.contains("skip.txt"), "find leaked non-match: {out:?}");
}

#[tokio::test]
async fn find_type_directory_filters() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "file.txt", "");
    fs::create_dir(dir.path().join("adir")).unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -type d").await;
    assert_eq!(code, 0, "find -type d should succeed: {out:?}");
    assert!(out.contains("adir"), "find -type d missed dir: {out:?}");
    assert!(!out.contains("file.txt"), "find -type d leaked file: {out:?}");
}

#[tokio::test]
async fn rm_glob_removes_only_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "x.tmp", "");
    touch(dir.path(), "y.tmp", "");
    touch(dir.path(), "keep.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "rm *.tmp").await;
    assert_eq!(code, 0, "rm glob should succeed: {out:?}");
    assert!(!dir.path().join("x.tmp").exists(), "x.tmp not removed");
    assert!(!dir.path().join("y.tmp").exists(), "y.tmp not removed");
    assert!(dir.path().join("keep.txt").exists(), "rm removed a non-match");
}

#[tokio::test]
async fn tac_reverses_line_order() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "seq.txt", "one\ntwo\nthree\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tac seq.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.lines().collect::<Vec<_>>(), vec!["three", "two", "one"], "tac: {out:?}");
}

#[tokio::test]
async fn uniq_collapses_adjacent_duplicates() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "d.txt", "a\na\nb\na\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "uniq d.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.lines().collect::<Vec<_>>(), vec!["a", "b", "a"], "uniq: {out:?}");
}

#[tokio::test]
async fn tee_writes_to_multiple_targets() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo payload | tee one.txt two.txt").await;
    assert_eq!(code, 0, "tee should succeed: {out:?}");
    assert_eq!(fs::read_to_string(dir.path().join("one.txt")).unwrap().trim(), "payload");
    assert_eq!(fs::read_to_string(dir.path().join("two.txt")).unwrap().trim(), "payload");
}

#[tokio::test]
async fn sed_substitutes_from_stdin() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo hello | sed 's/l/L/g'").await;
    assert_eq!(code, 0, "sed should succeed: {out:?}");
    assert_eq!(out, "heLLo");
}

#[tokio::test]
async fn base64_round_trips_through_pipeline() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo secret | base64 | base64 -d").await;
    assert_eq!(code, 0, "base64 roundtrip should succeed: {out:?}");
    assert_eq!(out, "secret");
}

#[tokio::test]
async fn ln_creates_symlink() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "target.txt", "data");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ln -s target.txt link.txt").await;
    assert_eq!(code, 0, "ln -s should succeed: {out:?}");
    let meta = fs::symlink_metadata(dir.path().join("link.txt")).expect("link exists");
    assert!(meta.file_type().is_symlink(), "link.txt should be a symlink");
}

#[tokio::test]
async fn glob_builtin_lists_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "m1.dat", "");
    touch(dir.path(), "m2.dat", "");
    touch(dir.path(), "no.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "glob '*.dat'").await;
    assert_eq!(code, 0);
    assert!(out.contains("m1.dat") && out.contains("m2.dat"), "glob: {out:?}");
    assert!(!out.contains("no.txt"), "glob leaked non-match: {out:?}");
}

#[tokio::test]
async fn checksum_emits_known_sha256() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "c.bin", "abc");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "checksum c.bin").await;
    assert_eq!(code, 0, "checksum should succeed: {out:?}");
    // sha256("abc")
    assert!(
        out.contains("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
        "checksum is not sha256(abc): {out:?}"
    );
    assert!(out.contains("c.bin"), "checksum missing filename: {out:?}");
}

#[tokio::test]
async fn grep_searches_all_files_and_prefixes_filenames() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "match here\nother\n");
    touch(dir.path(), "b.txt", "no\nmatch too\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "grep match a.txt b.txt").await;
    assert_eq!(code, 0, "grep should succeed: {out:?}");
    assert!(out.contains("match here"), "missed a.txt match: {out:?}");
    assert!(
        out.contains("match too"),
        "missed b.txt match — only searched the first file? {out:?}"
    );
    // Multi-file grep must indicate which file each match came from.
    assert!(
        out.contains("a.txt") && out.contains("b.txt"),
        "multi-file grep should prefix filenames: {out:?}"
    );
}

#[tokio::test]
async fn grep_glob_searches_all_matched_files() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.log", "needle\n");
    touch(dir.path(), "two.log", "needle\n");
    touch(dir.path(), "skip.txt", "needle\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "grep needle *.log").await;
    assert_eq!(code, 0, "grep glob should succeed: {out:?}");
    assert!(out.contains("one.log") && out.contains("two.log"), "grep glob: {out:?}");
    assert!(!out.contains("skip.txt"), "grep glob leaked non-.log file: {out:?}");
}

// ===========================================================================
// Batch 3 — multi-file text filters that already handle all positionals
// correctly (regression guards), plus stdin filters. These lock in behavior
// the direct-.execute() tests never exercised through the kernel.
// ===========================================================================

#[tokio::test]
async fn sort_merges_and_orders_multiple_files() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "f1.txt", "b\nd\n");
    touch(dir.path(), "f2.txt", "a\nc\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort f1.txt f2.txt").await;
    assert_eq!(code, 0, "sort multi-file should succeed: {out:?}");
    assert_eq!(out.lines().collect::<Vec<_>>(), vec!["a", "b", "c", "d"], "sort: {out:?}");
}

#[tokio::test]
async fn head_multi_file_emits_filename_headers() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "f1.txt", "1a\n1b\n");
    touch(dir.path(), "f2.txt", "2a\n2b\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "head -n 1 f1.txt f2.txt").await;
    assert_eq!(code, 0, "head multi-file should succeed: {out:?}");
    assert!(out.contains("f1.txt") && out.contains("f2.txt"), "head headers: {out:?}");
    assert!(out.contains("1a") && out.contains("2a"), "head content: {out:?}");
    assert!(!out.contains("1b"), "head should bound to 1 line/file: {out:?}");
}

#[tokio::test]
async fn wc_no_flag_reports_lines_words_bytes_per_file_plus_total() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "f1.txt", "a\nb\n"); // 2 lines
    touch(dir.path(), "f2.txt", "c\nd\ne\n"); // 3 lines
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "wc f1.txt f2.txt").await;
    assert_eq!(code, 0, "wc multi-file should succeed: {out:?}");
    assert!(out.contains("f1.txt") && out.contains("f2.txt"), "wc per-file rows: {out:?}");
    assert!(out.contains("total"), "wc should emit a total row: {out:?}");
    assert!(out.contains('5'), "wc total should be 5 lines: {out:?}");
}

#[tokio::test]
async fn uniq_count_flag_prefixes_counts() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "printf 'a\\na\\nb\\n' | uniq -c").await;
    assert_eq!(code, 0, "uniq -c should succeed: {out:?}");
    assert!(out.contains('2') && out.contains('a'), "uniq -c missing count for a: {out:?}");
    assert!(out.contains('1') && out.contains('b'), "uniq -c missing count for b: {out:?}");
}

#[tokio::test]
async fn tr_deletes_quoted_range_from_stdin() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo a1b2c3 | tr -d '0-9'").await;
    assert_eq!(code, 0, "tr -d should succeed: {out:?}");
    assert_eq!(out, "abc", "tr -d should strip digits: {out:?}");
}

#[tokio::test]
async fn tr_translates_letter_range() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo hello | tr a-z A-Z").await;
    assert_eq!(code, 0);
    assert_eq!(out, "HELLO", "tr range: {out:?}");
}

#[tokio::test]
async fn awk_prints_selected_field() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "printf 'x y z\\n' | awk '{print $2}'").await;
    assert_eq!(code, 0, "awk should succeed: {out:?}");
    assert_eq!(out, "y", "awk field: {out:?}");
}

// --- awk -F: field separator (P1 #1 in docs/awk-overhaul.md) ----------------
//
// `awk -F: '{print $1}'` is the most common awk invocation. Before the fix,
// kaish's lexer emitted `-F` as ShortFlag("F") and `:` as a separate Colon
// token, causing a parse error. The fix fuses span-adjacent metachars onto
// short flags so `-F:` → ShortFlag("F:"), which the existing glued-value
// binding already handles (same path as `cut -f1`).
//
// Expected output matches gawk 5.x. No external binary required at test time.

#[tokio::test]
async fn awk_field_separator_colon_file() {
    let dir = tempdir().unwrap();
    // passwd-style fixture: two colon-separated records
    touch(dir.path(), "pw.txt", "root:x:0:0\nuser:x:1000:1000\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "awk -F: '{print $1}' pw.txt").await;
    assert_eq!(code, 0, "awk -F: should succeed: {out:?}");
    assert_eq!(out, "root\nuser", "awk -F: first field: {out:?}");
}

#[tokio::test]
async fn awk_field_separator_colon_stdin() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "printf 'root:x:0:0\\nuser:x:1000:1000\\n' | awk -F: '{print $1}'").await;
    assert_eq!(code, 0, "awk -F: stdin should succeed: {out:?}");
    assert_eq!(out, "root\nuser", "awk -F: stdin first field: {out:?}");
}

#[tokio::test]
async fn awk_field_separator_semicolon() {
    // `-F;` requires the `;` to be a separate word (quoting it, or passing it as `-F ';'`).
    // A bare `awk -F;` fused the `;` as a shell statement terminator before this fix;
    // now `-F` and `;` stay separate tokens, so the field separator value must be
    // passed as a distinct quoted argument: `awk -F ';'`.
    let dir = tempdir().unwrap();
    touch(dir.path(), "semi.txt", "a;b;c\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "awk -F ';' '{print $2}' semi.txt").await;
    assert_eq!(code, 0, "awk -F ';' should succeed: {out:?}");
    assert_eq!(out, "b", "awk -F ';' second field: {out:?}");
}

#[tokio::test]
async fn awk_field_separator_long_flag() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "pw.txt", "root:x:0:0\nuser:x:1000:1000\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "awk --field-separator=: '{print $1}' pw.txt").await;
    assert_eq!(code, 0, "awk --field-separator=: should succeed: {out:?}");
    assert_eq!(out, "root\nuser", "awk --field-separator=: first field: {out:?}");
}

#[tokio::test]
async fn realpath_resolves_multiple_operands() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "p.txt", "");
    touch(dir.path(), "q.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "realpath p.txt q.txt").await;
    assert_eq!(code, 0, "realpath multi should succeed: {out:?}");
    assert!(out.contains("p.txt") && out.contains("q.txt"), "realpath: {out:?}");
}

#[tokio::test]
async fn printf_formats_int_and_string() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "printf '%d-%s' 42 hi").await;
    assert_eq!(code, 0, "printf should succeed: {out:?}");
    assert_eq!(out, "42-hi", "printf format: {out:?}");
}

// --- date: production binding + footgun regressions ------------------------
//
// These go through the full kernel (lex → parse → schema-bound argv → clap), so
// they lock in behaviors the in-module unit tests (clean argv) can't: the
// schema binding of a spaces-bearing `-d` value, and that the three documented
// footguns stay closed. Only clock-independent forms are asserted exactly; the
// kernel uses the real system clock.

#[tokio::test]
async fn date_at_epoch_decodes_through_kernel() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "date -u -d \"@1700000000\"").await;
    assert_eq!(code, 0, "@N decode should succeed: {out:?}");
    assert_eq!(out, "2023-11-14 22:13:20", "@N should decode, not echo: {out:?}");
}

#[tokio::test]
async fn date_nanos_specifier_translates_through_kernel() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "date -u -d \"@1700000000\" +%s%N").await;
    assert_eq!(code, 0, "%N should work, not panic: {out:?}");
    assert_eq!(out, "1700000000000000000", "%N → nanoseconds: {out:?}");
}

#[tokio::test]
async fn date_unknown_specifier_is_loud_not_panic() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "date +%Q").await;
    assert_eq!(code, 2, "unknown specifier must exit 2, never panic");
}

#[tokio::test]
async fn date_tz_is_honored_through_kernel() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "date --tz Asia/Tokyo -d \"@1700000000\" +%H:%M").await;
    assert_eq!(code, 0, "tz date should succeed: {out:?}");
    assert_eq!(out, "07:13", "Tokyo is +09:00 from 22:13Z: {out:?}");
}

#[tokio::test]
async fn date_relative_with_spaces_binds_through_kernel() {
    // The schema-driven binding risk: a quoted `-d` value with spaces must
    // reach clap as one argument. The kernel path can't inject a fixed clock,
    // so pin the answer to an independently-computed expected value — a
    // length/dash-count check would pass even if "2 weeks ago" silently
    // degraded to "today".
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "date -u -d \"2 weeks ago\" +%Y-%m-%d").await;
    assert_eq!(code, 0, "spaces in -d must bind as one value: {out:?}");
    let expected = (chrono::Utc::now() - chrono::Duration::weeks(2))
        .format("%Y-%m-%d")
        .to_string();
    assert_eq!(out, expected, "should be exactly two weeks before today: {out:?}");
}

#[tokio::test]
async fn date_absolute_plus_offset_through_kernel() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "date -u -d \"2026-06-01 -1 day\" +%Y-%m-%d").await;
    assert_eq!(code, 0, "absolute+offset should succeed: {out:?}");
    assert_eq!(out, "2026-05-31", "last day of previous month: {out:?}");
}

#[tokio::test]
async fn date_json_emits_fields_through_kernel() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    // Pin epoch↔iso↔utc consistency at one instant rather than just checking
    // field types (which would pass with epoch:0 / a wrong weekday).
    let (out, code) = run(&kernel, "date -u -d \"@1700000000\" --json").await;
    assert_eq!(code, 0, "date --json should succeed: {out:?}");
    let parsed: serde_json::Value = serde_json::from_str(&out).expect("date --json is JSON");
    assert_eq!(parsed["epoch"], 1_700_000_000_i64, "{out:?}");
    assert_eq!(parsed["iso"], "2023-11-14T22:13:20+00:00", "{out:?}");
    assert_eq!(parsed["utc"], "2023-11-14T22:13:20Z", "{out:?}");
    assert_eq!(parsed["weekday"], "Tuesday", "{out:?}");
    assert_eq!(parsed["offset_seconds"], 0, "{out:?}");
}

// --- Negative cases: agents branch on exit codes, so a builtin must fail with
// the right code AND a message naming the cause. These pin the specific code +
// an error substring; a regression to a happy-path swallow would break them.
// (See docs/issues.md: builtin_kernel_tests was 100% happy-path.)

/// Run a script and return (trimmed stdout, stderr, exit code).
async fn run_err(kernel: &kaish_kernel::Kernel, script: &str) -> (String, String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (
        result.text_out().trim().to_string(),
        result.err.clone(),
        result.code,
    )
}

#[tokio::test]
async fn cat_missing_file_errors_with_name() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, err, code) = run_err(&kernel, "cat nope.txt").await;
    assert_eq!(code, 1, "missing file → exit 1; err={err:?}");
    assert!(err.contains("nope.txt"), "err should name the file: {err:?}");
}

#[tokio::test]
async fn wc_missing_file_errors_with_name() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, err, code) = run_err(&kernel, "wc nope.txt").await;
    assert_eq!(code, 1, "missing file → exit 1; err={err:?}");
    assert!(err.contains("nope.txt"), "err should name the file: {err:?}");
}

#[tokio::test]
async fn sort_missing_file_errors_with_name() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, err, code) = run_err(&kernel, "sort nope.txt").await;
    assert_eq!(code, 1, "missing file → exit 1; err={err:?}");
    assert!(err.contains("nope.txt"), "err should name the file: {err:?}");
}

#[tokio::test]
async fn cut_without_field_or_char_spec_errors() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.txt", "a b c\n");
    let kernel = kernel_at(dir.path());
    let (_out, err, code) = run_err(&kernel, "cut data.txt").await;
    assert_eq!(code, 1, "no -f/-c → exit 1; err={err:?}");
    assert!(err.contains("-f or -c"), "err should explain the missing spec: {err:?}");
}

#[tokio::test]
async fn head_unknown_flag_is_clap_usage_error() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.txt", "x\n");
    let kernel = kernel_at(dir.path());
    let (_out, err, code) = run_err(&kernel, "head --bogus data.txt").await;
    assert_eq!(code, 2, "unknown flag → clap usage error exit 2; err={err:?}");
    assert!(err.contains("bogus"), "err should name the bad flag: {err:?}");
}

#[tokio::test]
async fn grep_missing_pattern_only_no_match_is_exit_1() {
    // A readable file with no match is the canonical exit-1 (not an error code).
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.txt", "hello\n");
    let kernel = kernel_at(dir.path());
    let (_out, _err, code) = run_err(&kernel, "grep zzz data.txt").await;
    assert_eq!(code, 1, "no match → exit 1");
}
