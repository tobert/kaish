//! Kernel-routed fidelity fixes from the builtin sweep (docs/builtin-sweep-
//! overhaul.md, P2/P3). Each asserts kaish output == the panel's banked
//! consensus, driven through `kernel.execute_with_options(...)` so the full
//! lex → parse → dispatch → builtin path runs (not a builtin's `.execute()`).
//!
//! Oracle = model-consensus, NOT GNU (`[[model-memory-over-gnu-oracle]]`).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_latch(false).with_trash(false))
        .expect("failed to create kernel")
}

async fn run(prog: &str, stdin: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel
        .execute_with_options(prog, ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ─────────────────────────── tail -n +N (P2.2) ───────────────────────────
// `tail -n +N` starts *at* line N (1-based), not "last N lines". The leading
// `+` was lost (clap/`parse` strips it), so `+2` was silently treated as `2`.

#[tokio::test]
async fn tail_plus_n_starts_from_line_n() {
    let (out, code) = run("tail -n +2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "beta\ncherry\ndate\n", "tail -n +2 = from line 2 to end");
}

#[tokio::test]
async fn tail_plus_one_is_whole_input() {
    let (out, code) = run("tail -n +1", "alpha\nbeta\ncherry\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\ncherry\n", "tail -n +1 = entire input");
}

#[tokio::test]
async fn tail_plus_n_past_end_is_empty() {
    let (out, code) = run("tail -n +9", "alpha\nbeta\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "start past EOF yields nothing");
}

#[tokio::test]
async fn tail_negative_n_still_means_last_n() {
    // Regression guard: the plain "last N" form must be unaffected.
    let (out, code) = run("tail -n 2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cherry\ndate\n", "tail -n 2 = last 2 lines");
}

// ─────────────────────────── head -n -N (P2.3) ───────────────────────────
// `head -n -N` prints all lines BUT the last N. The negative count used to
// wrap (`-1 as usize`) and emit everything.

#[tokio::test]
async fn head_negative_n_drops_last_n() {
    let (out, code) = run("head -n -1", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\ncherry\n", "head -n -1 = all but last line");
}

#[tokio::test]
async fn head_negative_n_dropping_all_is_empty() {
    let (out, code) = run("head -n -9", "alpha\nbeta\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "dropping more than present yields nothing");
}

#[tokio::test]
async fn head_positive_n_still_means_first_n() {
    // Regression guards: explicit `-n N` and POSIX shorthand `-N` unaffected.
    let (out, code) = run("head -n 2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\n", "head -n 2 = first 2 lines");

    let (out2, code2) = run("head -2", "alpha\nbeta\ncherry\n").await;
    assert_eq!(code2, 0, "out={out2:?}");
    assert_eq!(out2, "alpha\nbeta\n", "head -2 shorthand = first 2 lines");
}

// ─────────────────── cut: line with no delimiter (P2.4) ───────────────────
// `cut -f` (no `-s`) passes a line that lacks the delimiter through unchanged;
// it used to emit an empty line. `-s` suppresses non-delimited lines.

#[tokio::test]
async fn cut_passes_through_line_without_delimiter() {
    let (out, code) = run("cut -d, -f2", "a,b,c\nnodelim\nx,y,z\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "b\nnodelim\ny\n", "non-delimited line passes through whole");
}

#[tokio::test]
async fn cut_s_suppresses_line_without_delimiter() {
    let (out, code) = run("cut -s -d, -f2", "a,b,c\nnodelim\nx,y,z\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "b\ny\n", "-s drops the non-delimited line entirely");
}

// ─────────────────────── split --limit=N (P2.6) ──────────────────────────
// `--limit=N` caps the result at N fields (N-1 splits); the remainder stays
// intact. It used to produce N+1 fields (`splitn(limit + 1)`).

async fn run_no_stdin(prog: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel.execute(prog).await.expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

#[tokio::test]
async fn split_limit_caps_field_count() {
    // 2 fields: first split, remainder intact.
    let (out, code) = run_no_stdin(r#"split "a:b:c:d" ":" --limit=2"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb:c:d\n", "--limit=2 = at most 2 fields");
}

#[tokio::test]
async fn split_limit_one_is_whole_input() {
    let (out, code) = run_no_stdin(r#"split "a:b:c" ":" --limit=1"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a:b:c\n", "--limit=1 = the whole string, unsplit");
}

#[tokio::test]
async fn split_no_limit_splits_all() {
    // Regression guard: unlimited split unchanged.
    let (out, code) = run_no_stdin(r#"split "a:b:c:d" ":""#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb\nc\nd\n", "no limit = split every delimiter");
}

#[tokio::test]
async fn split_limit_whitespace() {
    let (out, code) = run_no_stdin(r#"split "a b c d" --limit=2"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb c d\n", "whitespace --limit=2 = at most 2 fields");
}

// ───────────────────────── jq -c (compact) (P2.5) ────────────────────────
// `-c` emits compact single-line JSON; it used to be silently ignored
// (pretty-printed). Output ends in a trailing newline (P4.1, folded here).

#[tokio::test]
async fn jq_compact_emits_single_line() {
    let (out, code) = run("jq -c .", "{\"a\":1,\"b\":[2,3]}\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "{\"a\":1,\"b\":[2,3]}\n", "-c = compact one-line JSON + newline");
}

#[tokio::test]
async fn jq_pretty_default_and_trailing_newline() {
    let (out, code) = run("jq .", "{\"a\":1}\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "{\n  \"a\": 1\n}\n", "default = pretty, terminated by newline");
}

#[tokio::test]
async fn jq_compact_array_iteration() {
    // -c over a stream still ends each value on its own line, newline-terminated.
    let (out, code) = run("jq -c '.[]'", "[{\"x\":1},{\"y\":2}]\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "{\"x\":1}\n{\"y\":2}\n", "compact per-value, newline-terminated");
}

// ─────────────────────────── wc format (P2.7) ────────────────────────────
// Single-count `wc -l/-w/-c` is the bare unpadded number + newline (was a
// leading tab and no trailing newline). `wc -l` counts newlines (W5): an
// unterminated final line is NOT a line.

#[tokio::test]
async fn wc_lines_is_bare_number_with_newline() {
    let (out, code) = run("wc -l", "a\nb\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "2\n", "wc -l = bare count + newline, no leading tab");
}

#[tokio::test]
async fn wc_lines_counts_newlines_not_final_unterminated_line() {
    // W5: "a\nb" has one newline → 1, not 2.
    let (out, code) = run("wc -l", "a\nb").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "wc -l counts newline chars, not str::lines segments");
}

#[tokio::test]
async fn wc_words_and_bytes_single_count() {
    let (out, code) = run("wc -w", "one two three\nfour five\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "5\n");

    let (outc, codec) = run("wc -c", "hello world\n").await;
    assert_eq!(codec, 0, "out={outc:?}");
    assert_eq!(outc, "12\n");
}

#[tokio::test]
async fn wc_all_counts_right_justified_space_separated() {
    // lines=2, words=5, bytes=24 → right-justified to common width, 1-space sep.
    let (out, code) = run("wc", "one two three\nfour five\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, " 2  5 24\n", "multi-count right-justified, space-separated");
}

// ──────────────────── tr -c / --complement (P2.8) ────────────────────────
// `-c` complements SET1 (operate on chars NOT in SET1). The common idiom is
// `-cd` (delete everything not in the set); it used to be a clap parse error.

#[tokio::test]
async fn tr_complement_delete_keeps_only_set() {
    let (out, code) = run("tr -cd '[:digit:]'", "a1b2c3\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "123", "tr -cd '[:digit:]' keeps only digits");
}

#[tokio::test]
async fn tr_complement_long_flag() {
    let (out, code) = run("tr --complement --delete '[:alpha:]'", "a1b2c3\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "abc", "--complement --delete keeps only letters");
}

#[tokio::test]
async fn tr_complement_translate_maps_non_set() {
    // Non-lowercase chars become '_'.
    let (out, code) = run("tr -c 'a-z' '_'", "ab12cd\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "ab__cd_", "complement translate maps non-set1 to set2");
}

#[tokio::test]
async fn tr_complement_squeeze_only_squeezes_the_replacement() {
    // -cs with a multi-char SET2: complement chars all become set2's last char
    // ('z') and squeeze; a pass-through set1 char that happens to be in SET2
    // ('x') must NOT be squeezed.
    let (out, code) = run("tr -cs 'a-z' 'xyz'", "xx12\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "xxz", "only the emitted replacement (z) squeezes, not pass-through x");
}

#[tokio::test]
async fn tr_plain_delete_still_works() {
    // Regression guard: non-complement delete unaffected.
    let (out, code) = run("tr -d '0-9'", "a1b2c3\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "abc\n", "tr -d removes the set (newline preserved)");
}
