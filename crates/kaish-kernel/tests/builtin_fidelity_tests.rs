//! Output-fidelity tests for the text builtins: each asserts that kaish emits
//! the expected bytes for a given invocation. Covers `tail -n +N` / `head -n
//! -N`, `cut` delimiter passthrough + `-s`, `split --limit`, `jq -c`, `wc`
//! formatting + newline counting, `tr -c`, `sort -V`, the trailing-newline
//! policy (tac/base64/xxd), and the unquoted-comma argv message.
//!
//! Driven through `kernel.execute_with_options(...)` so the full lex → parse →
//! dispatch → builtin path runs (not a builtin's `.execute()`). Expected
//! outputs are a cross-model banked consensus, NOT GNU coreutils
//! (`[[model-memory-over-gnu-oracle]]`). These pin behavior permanently; they
//! originated in the 2026-06 builtin sweep (scaffolding since removed).

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
async fn tail_plain_n_means_last_n() {
    // Regression guard: the plain "last N" form must be unaffected.
    let (out, code) = run("tail -n 2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cherry\ndate\n", "tail -n 2 = last 2 lines");
}

#[tokio::test]
async fn tail_explicit_negative_n_means_last_n() {
    // `tail -n -N` is the explicit "from the end" form — same as plain `-n N`.
    // The count arrives as `Int(-2)`; a bare `as usize` wrapped to ~usize::MAX
    // and `saturating_sub` skipped 0 lines, silently emitting the whole input.
    let (out, code) = run("tail -n -2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cherry\ndate\n", "tail -n -2 = last 2 lines");
}

#[tokio::test]
async fn tail_explicit_negative_c_means_last_bytes() {
    // Same wrapping bug in byte mode: `tail -c -3` must emit the last 3 bytes.
    let (out, code) = run("tail -c -3", "abcdef\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "ef\n", "tail -c -3 = last 3 bytes");
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

/// Run a program expected to fail to parse/validate; return the error text.
async fn run_err(prog: &str) -> String {
    let kernel = kernel();
    match kernel.execute(prog).await {
        Ok(r) => panic!("expected an error, got ok: {:?} (code {})", r.text_out(), r.code),
        Err(e) => format!("{e:?}"),
    }
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

// ────────────────── unquoted-comma message (P2.1 / P4.3) ─────────────────
// An unquoted comma in argv is intentionally LOUD (kaish reserves `,`), but
// the message must teach quoting — not say "token pasting" (nothing was
// pasted). The quoted idiom must work.

#[tokio::test]
async fn unquoted_comma_message_teaches_quoting() {
    let err = run_err("echo a,b").await;
    assert!(
        err.contains("comma") && err.to_lowercase().contains("quote"),
        "comma error should mention the comma and quoting, got: {err}"
    );
    assert!(
        !err.contains("token pasting"),
        "comma error must not use the misleading 'token pasting' text: {err}"
    );
}

#[tokio::test]
async fn quoted_comma_field_list_works() {
    // The fix is the message, not the grammar — quoting is how you pass a list.
    let (out, code) = run("cut -d: -f \"1,3\"", "a:b:c\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a:c\n");
}

#[tokio::test]
async fn non_comma_pasting_keeps_generic_message() {
    // The interpolation-splat case keeps its own hint (not the comma one).
    let err = run_err("echo $(echo x)y").await;
    assert!(
        err.contains("token pasting"),
        "non-comma adjacency keeps the token-pasting hint: {err}"
    );
}

// ─────────────────────────── sort -V (P3) ────────────────────────────────
// Version sort: numeric runs compare by value, so v1.2 < v1.9 < v1.10.

#[tokio::test]
async fn sort_version_orders_numeric_runs_by_value() {
    let (out, code) = run("sort -V", "v1.10\nv1.2\nv1.9\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "v1.2\nv1.9\nv1.10\n");
}

#[tokio::test]
async fn sort_version_multi_component() {
    let (out, code) = run("sort -V", "1.0.10\n1.0.2\n1.0.1\n1.10.0\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1.0.1\n1.0.2\n1.0.10\n1.10.0\n");
}

#[tokio::test]
async fn sort_version_reverse() {
    let (out, code) = run("sort -rV", "v1.10\nv1.2\nv1.9\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "v1.10\nv1.9\nv1.2\n");
}

#[tokio::test]
async fn sort_version_prefix_sorts_first() {
    // A string that is a prefix of another sorts before it (`v1` < `v1.0`).
    let (out, code) = run("sort -V", "v1.0\nv1\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "v1\nv1.0\n");
}

#[tokio::test]
async fn sort_version_unique_dedups() {
    let (out, code) = run("sort -Vu", "1.10\n1.2\n1.2\n1.10\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1.2\n1.10\n");
}

#[tokio::test]
async fn sort_version_no_digits_is_lexical() {
    let (out, code) = run("sort -V", "banana\napple\ncherry\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "apple\nbanana\ncherry\n");
}

// ───────────────── trailing-newline EMIT batch (P4.1) ────────────────────
// tac / base64-encode / xxd -p newline-terminate their output to match the
// consensus and kaish's line tools. base64 -d (raw bytes out) does NOT.

#[tokio::test]
async fn tac_emits_trailing_newline() {
    let (out, code) = run("tac", "a\nb\nc\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "c\nb\na\n");
}

#[tokio::test]
async fn base64_encode_emits_trailing_newline() {
    let (out, code) = run("base64", "hello").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "aGVsbG8=\n");
}

#[tokio::test]
async fn base64_decode_stays_raw_no_added_newline() {
    let (out, code) = run("base64 -d", "aGVsbG8=").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "hello", "decoded bytes are emitted verbatim, no newline added");
}

#[tokio::test]
async fn xxd_plain_emits_trailing_newline() {
    let (out, code) = run("xxd -p", "hi").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "6869\n");
}

#[tokio::test]
async fn xxd_plain_30_byte_wrap_boundary_no_double_newline() {
    // `xxd -p` wraps every 30 bytes; an exact multiple must not double-terminate.
    let input = "a".repeat(30);
    let (out, code) = run("xxd -p", &input).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, format!("{}\n", "61".repeat(30)), "single trailing newline");
    assert_eq!(out.matches('\n').count(), 1, "exactly one newline");
}

#[tokio::test]
async fn xxd_plain_31_bytes_wraps_then_terminates() {
    let input = "a".repeat(31);
    let (out, code) = run("xxd -p", &input).await;
    assert_eq!(code, 0, "out={out:?}");
    // 30 bytes on line 1 (wrap newline), 1 byte on line 2 (terminal newline).
    assert_eq!(out, format!("{}\n61\n", "61".repeat(30)));
    assert_eq!(out.matches('\n').count(), 2, "wrap newline + terminator");
}

// ─────────────────── awk numeric-string (strnum) comparison ───────────────────
// POSIX: a field/`split`/`-v`/command-line value is a *numeric string* — it
// compares numerically only when it *looks* like a number. A string *constant*
// never does. The old `compare_values` used `l_num || r_num`, forcing a numeric
// compare whenever *either* side was numeric, so a non-numeric field (or string
// constant) silently coerced to `0.0`. Oracle: gawk 5.4.0.

#[tokio::test]
async fn awk_field_nonnumeric_vs_zero_is_string_compare() {
    // `$1 == 0` with $1 = "abc": strnum "abc" doesn't look numeric → string
    // compare → "abc" != "0" → NE. (Old `||` path coerced "abc"→0.0 → EQ.)
    let (out, code) = run(r#"awk '{ if ($1 == 0) print "EQ"; else print "NE" }'"#, "abc\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "NE\n");
}

#[tokio::test]
async fn awk_field_numericlooking_vs_zero_is_numeric_compare() {
    // $1 = "0" is a numeric string → numeric compare → 0 == 0 → EQ. Regression
    // guard: the strnum fix must not break the genuinely-numeric field case.
    let (out, code) = run(r#"awk '{ if ($1 == 0) print "EQ"; else print "NE" }'"#, "0\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "EQ\n");
}

#[tokio::test]
async fn awk_string_constant_vs_zero_is_string_compare() {
    // A string *constant* is never numeric: "abc" == 0 → string compare → NE.
    let (out, code) = run(r#"awk 'BEGIN { if ("abc" == 0) print "EQ"; else print "NE" }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "NE\n");
}

#[tokio::test]
async fn awk_two_numeric_fields_compare_numerically() {
    // Both fields are numeric strings → numeric compare → 10 > 9. Regression
    // guard for the common `$1 > $2` idiom.
    let (out, code) = run(r#"awk '{ if ($1 > $2) print "GT"; else print "LE" }'"#, "10 9\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "GT\n");
}

#[tokio::test]
async fn awk_string_constants_compare_lexically() {
    // Two string constants → lexical compare → "10" < "9".
    let (out, code) = run(r#"awk 'BEGIN { if ("10" < "9") print "LT"; else print "GE" }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "LT\n");
}

// ─────────────────────────── awk length(array) ───────────────────────────
// `length(arr)` returns the element count. The old code eval'd the array name
// as a scalar → Uninitialized → "" → length 0.

#[tokio::test]
async fn awk_length_of_array_is_element_count() {
    let (out, code) = run(r#"awk 'BEGIN { a[1]=1; a[2]=2; a[3]=3; print length(a) }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "3\n");
}

#[tokio::test]
async fn awk_length_bare_still_measures_record() {
    // Regression guard: bare `length` (on $0) is unaffected by the array case.
    let (out, code) = run(r#"awk '{ print length }'"#, "hello\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "5\n");
}

// ─────────────────────────── awk exit N ───────────────────────────
// `exit N` must set the builtin's exit code; END blocks still run.

#[tokio::test]
async fn awk_exit_code_propagates() {
    let (out, code) = run(r#"awk 'BEGIN { exit 3 }'"#, "").await;
    assert_eq!(code, 3, "out={out:?}");
    assert_eq!(out, "");
}

#[tokio::test]
async fn awk_exit_runs_end_and_keeps_code() {
    let (out, code) = run(r#"awk '{ exit 2 } END { print "end ran" }'"#, "x\n").await;
    assert_eq!(code, 2, "out={out:?}");
    assert_eq!(out, "end ran\n");
}

#[tokio::test]
async fn awk_bare_exit_in_end_preserves_prior_code() {
    // A bare `exit` (no expression) keeps the most recently set status (POSIX),
    // it does not reset it to 0. `exit 7` in BEGIN, then bare `exit` in END → 7.
    let (out, code) = run(r#"awk 'BEGIN { exit 7 } END { exit }'"#, "").await;
    assert_eq!(code, 7, "bare exit must not clobber the prior code, out={out:?}");
}

// ───────────────── awk out-of-range field is the empty string ─────────────────
// A referenced-but-absent field is the string "" (string compare), not a numeric
// 0 — `$5 == 0` is false, distinct from an unset *variable*.

#[tokio::test]
async fn awk_out_of_range_field_is_empty_string_not_zero() {
    let (out, code) = run(r#"awk '{ print ($5 == 0) }'"#, "a b\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "0\n", "$5 (absent) == 0 is a string compare → false");
}

#[tokio::test]
async fn awk_out_of_range_field_equals_empty() {
    let (out, code) = run(r#"awk '{ print ($5 == "") }'"#, "a b\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "$5 (absent) == \"\" is true");
}

// ─────────────────────────── awk multiple -v ───────────────────────────
// Every `-v NAME=VALUE` is applied (clap kept only the last before).

#[tokio::test]
async fn awk_multiple_v_assignments_all_apply() {
    let (out, code) = run(r#"awk -v a=1 -v b=2 'BEGIN { print a, b }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1 2\n");
}

// ─────────────────────── awk $0 reassignment re-splits ───────────────────────
// Assigning `$0` must re-split into $1..$NF and update NF.

#[tokio::test]
async fn awk_assigning_dollar_zero_resplits_fields() {
    let (out, code) = run(r#"awk '{ $0 = "x y z"; print NF, $2 }'"#, "one two\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "3 y\n");
}

// ─────────────────── awk invalid FS regex is a loud error ───────────────────
// A multi-char invalid regex FS must error, not silently fall back to a literal
// split (which yields silently-wrong field counts). gawk: fatal. Single-char FS
// stays literal (that matches gawk and is *not* a regex).

#[tokio::test]
async fn awk_invalid_multichar_fs_regex_errors() {
    let (out, code) = run(r#"awk -F 'a[' '{ print NF }'"#, "xa[y\n").await;
    assert_ne!(code, 0, "invalid FS regex must be a loud error, out={out:?}");
    assert_eq!(out, "", "no field output on a bad-FS error");
}

#[tokio::test]
async fn awk_invalid_split_regex_errors() {
    let (out, code) = run(r#"awk 'BEGIN { n = split("xa[y", arr, "a["); print n }'"#, "").await;
    assert_ne!(code, 0, "invalid split() regex must be a loud error, out={out:?}");
}

// ─────────────────── head on an empty pipe emits nothing ───────────────────
// The streaming path built `format!("{}\n", lines.join("\n"))` with no
// is_empty guard, so `true | head` produced "\n" instead of "".

#[tokio::test]
async fn head_empty_pipe_emits_no_newline() {
    let (out, code) = run("true | head", "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "empty input yields empty output, not a bare newline");
}

// ─────────────────── kaish-validate -w gates warnings ───────────────────
// Warnings (e.g. an unknown command) show only with `-w`; without it the
// script reads as `valid`. The old `A || B || !A` made `-w` inert (always on).

#[tokio::test]
async fn validate_suppresses_warnings_without_w() {
    let (out, code) = run("kaish-validate -e 'notacommand'", "").await;
    assert_eq!(code, 0, "warning-only script is still valid, out={out:?}");
    assert!(out.contains("valid"), "default (no -w) suppresses warnings: {out:?}");
    assert!(!out.contains("not found"), "warning must be hidden without -w: {out:?}");
}

#[tokio::test]
async fn validate_shows_warnings_with_w() {
    let (out, code) = run("kaish-validate -w -e 'notacommand'", "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(out.contains("not found"), "-w surfaces the warning: {out:?}");
}

// ─────────────────── jq rejects a non-UTF-8 input file loudly ───────────────────
// The file-read path used `from_utf8_lossy`, silently U+FFFD-corrupting binary
// before the JSON parse. It must decode strictly and error like the stdin path.

#[tokio::test]
async fn jq_non_utf8_file_errors_loudly() {
    use std::io::Write;
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("bad.json");
    std::fs::File::create(&path).unwrap().write_all(&[0xff, 0xfe, 0xfd]).unwrap();

    // Assert on the *error channel*, not just the exit code: with the lossy
    // fallback the bytes become U+FFFD and then fail JSON parsing (also exit 1,
    // empty stdout), so a code/out-only check passes even with the bug present.
    // The distinguishing signal is the "invalid UTF-8" message vs "invalid JSON".
    let kernel = kernel();
    let result = kernel
        .execute(&format!("jq . {}", path.display()))
        .await
        .expect("kernel execute");
    assert_ne!(result.code, 0, "non-UTF-8 file must be a loud error: {result:?}");
    assert!(result.text_out().is_empty(), "no lossy/parsed output: {:?}", result.text_out());
    assert!(
        result.err.contains("invalid UTF-8"),
        "must report the decode failure, not a confusing JSON-parse error: {:?}",
        result.err
    );
}

// ───────────────── jq --arg (consumes>1) keeps both value slots ─────────────────
// Regression guard for the arg-binder change: a multi-value flag (`--arg NAME
// VAL`, consumes 2) must NOT treat a following `key=value` token as a value (only
// single-value flags do), or it would steal the filter into the second slot.

#[tokio::test]
async fn jq_arg_two_value_form_still_binds_name_and_value() {
    let (out, code) = run(r#"jq --arg name kaish -n '$name'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "\"kaish\"", "--arg NAME VAL binds both slots: {out:?}");
}
