//! Pre-execution validation tests for `jq` (E007 InvalidJqFilter) and
//! `sed` (E006 InvalidSedExpr).
//!
//! All tests are kernel-routed: they drive a command string through
//! `kernel.execute()` so the full lex → parse → validate → dispatch pipeline
//! runs, matching the real agent experience.

#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;

// ============================================================================
// jq — E007 InvalidJqFilter
// ============================================================================

/// A clearly-malformed filter (`.[`) is rejected with E007 before execution.
#[tokio::test]
async fn jq_invalid_filter_blocks_with_e007() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute(r#"echo '{}' | jq '.['  "#)
        .await
        .expect_err("invalid jq filter should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E007"), "should name the code: {msg}");
}

/// A syntactically valid filter over real JSON passes validation and runs.
#[tokio::test]
async fn jq_valid_filter_passes_validation_and_runs() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo '{"x":1}' | jq '.x'"#)
        .await
        .expect("valid jq filter should pass validation");
    assert!(result.ok(), "jq .x should succeed: {:?}", result.err);
    assert!(result.text_out().contains('1'), "output should contain 1");
}

/// A filter that uses `$x` with a matching `--arg x 1` binding must NOT
/// produce E007 — the validator must compile with the declared var names.
#[tokio::test]
async fn jq_bound_var_filter_does_not_false_error() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo '{"foo":1}' | jq --arg x hello '.foo | tostring + $x'"#)
        .await
        .expect("filter with --arg binding should pass validation");
    assert!(result.ok(), "should succeed at runtime: {:?}", result.err);
}

/// The `--arg=NAME` equals form is not a legal jq invocation (real jq rejects it
/// as "Unknown option"), so kaish does not special-case it: the command is
/// rejected before execution rather than slipping through to a runtime error.
#[tokio::test]
async fn jq_equals_form_arg_is_rejected_not_silently_run() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute(r#"echo '{"foo":1}' | jq --arg=x hello '$x'"#)
        .await
        .expect_err("illegal --arg=NAME form should not succeed");
    assert!(
        err.to_string().contains("E007"),
        "rejected at validation: {err}"
    );
}

/// A filter stored in a variable expands to `<dynamic>` at validation time —
/// the validator must skip and let runtime handle it.
#[tokio::test]
async fn jq_dynamic_filter_skips_validation() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // F contains a valid filter, so the dynamic path should run successfully.
    let result = kernel
        .execute(r#"F='.x'; echo '{"x":42}' | jq $F"#)
        .await
        .expect("dynamic filter should pass validation and run");
    assert!(result.ok(), "dynamic filter should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "42",
        "should output 42: {}",
        result.text_out()
    );
}

// ============================================================================
// sed — E006 InvalidSedExpr
// ============================================================================

/// An unknown sed command letter is rejected with E006 before execution.
#[tokio::test]
async fn sed_unknown_command_blocks_with_e006() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute(r#"echo hello | sed 'zzz'"#)
        .await
        .expect_err("unknown sed command should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E006"), "should name the code: {msg}");
}

/// A valid substitution expression passes validation and runs.
#[tokio::test]
async fn sed_valid_expression_passes_validation_and_runs() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo hello | sed 's/hello/world/'"#)
        .await
        .expect("valid sed expression should pass validation");
    assert!(result.ok(), "sed s/hello/world/ should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "world",
        "should output 'world': {}",
        result.text_out()
    );
}

/// A sed expression in a variable is `<dynamic>` at validation time — skip.
#[tokio::test]
async fn sed_dynamic_expression_skips_validation() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // Use a valid expression so the dynamic path runs successfully.
    let result = kernel
        .execute(r#"F='s/a/b/'; echo 'abc' | sed $F"#)
        .await
        .expect("dynamic sed expression should pass validation and run");
    assert!(result.ok(), "dynamic sed should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "bbc",
        "should output 'bbc': {}",
        result.text_out()
    );
}

/// A file-based test to confirm the kernel is properly rooted (no system-path
/// coupling). Creates a real file and exercises sed on it end-to-end.
#[tokio::test]
async fn sed_valid_on_real_file_passes() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("data.txt"), "hello world\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("sed 's/world/kaish/' data.txt")
        .await
        .expect("sed on real file should pass validation");
    assert!(result.ok(), "sed on file should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "hello kaish",
        "should output 'hello kaish': {}",
        result.text_out()
    );
}

// ============================================================================
// sed — repeated `-e` expressions (docs/issues.md P1 regression guard)
// ============================================================================

/// `sed -e A -e B` must apply BOTH expressions, in order. The pre-fix kernel
/// overwrote the first `-e` in the `named` HashMap (a "never silently corrupt"
/// violation), so only the last expression ran. This is the canonical
/// regression test for the repeatable-flag accumulation fix.
#[tokio::test]
async fn sed_two_e_flags_apply_both_in_order() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo 'abc' | sed -e 's/a/X/' -e 's/c/Z/'"#)
        .await
        .expect("two -e expressions should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "XbZ",
        "both expressions must apply: {}",
        result.text_out()
    );
}

/// Ordering matters: a later `-e` sees the earlier one's output. `s/a/b/` then
/// `s/b/c/` over `a` yields `c`, proving sequential application (not just
/// "both ran independently").
#[tokio::test]
async fn sed_two_e_flags_pipe_through_in_sequence() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo 'a' | sed -e 's/a/b/' -e 's/b/c/'"#)
        .await
        .expect("chained -e expressions should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(result.text_out().trim(), "c", "got: {}", result.text_out());
}

/// Three `-e` flags all survive (not just two) — guards against an off-by-one
/// in the accumulation.
#[tokio::test]
async fn sed_three_e_flags_all_apply() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo 'abc' | sed -e 's/a/1/' -e 's/b/2/' -e 's/c/3/'"#)
        .await
        .expect("three -e expressions should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(result.text_out().trim(), "123", "got: {}", result.text_out());
}

/// The `--expression=VALUE` form must accumulate exactly like the `-e VALUE`
/// space form. The pre-fix `Arg::Named` path did an unconditional HashMap
/// insert, so `--expression=A --expression=B` silently kept only B — the same
/// "never silently corrupt" violation the `-e` fix closed, leaking through a
/// sibling door.
#[tokio::test]
async fn sed_two_long_expression_eq_flags_apply_both() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo 'abc' | sed --expression='s/a/X/' --expression='s/c/Z/'"#)
        .await
        .expect("two --expression= flags should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "XbZ",
        "both --expression= values must apply: {}",
        result.text_out()
    );
}

/// Mixing the `=` form and the space form must converge on one ordered array,
/// not clobber each other (the `=` form used to overwrite the `-e` array, or
/// hard-error when `-e` came second and tried to push into a String).
#[tokio::test]
async fn sed_mixed_e_and_long_expression_eq_apply_in_order() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo 'abc' | sed -e 's/a/X/' --expression='s/b/Y/'"#)
        .await
        .expect("mixed -e and --expression= should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "XYc",
        "both forms must apply in order: {}",
        result.text_out()
    );
}

// ============================================================================
// sed — ergonomics pass (LLM-panel-driven gap closures, 2026-06-15)
// Scenarios are the exact forms Gemini-flash and Claude-haiku reached for.
// ============================================================================

/// `;` chains commands (both models used it for multi-pattern delete). Pre-fix
/// kaish silently dropped everything after the first `;`.
#[tokio::test]
async fn sed_semicolon_chains_commands() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo 'abc' | sed 's/a/X/; s/c/Z/'"#)
        .await
        .expect("; chain should run");
    assert!(result.ok(), "{:?}", result.err);
    assert_eq!(result.text_out().trim(), "XbZ");
}

/// `s///N` replaces the Nth match (both models used `s/x/Y/2`). Pre-fix kaish
/// silently replaced the first.
#[tokio::test]
async fn sed_nth_occurrence_substitute() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo 'aaa' | sed 's/a/X/2'"#)
        .await
        .expect("s///2 should run");
    assert_eq!(result.text_out().trim(), "aXa", "{:?}", result.err);
}

/// `a`/`i` append/insert lines (both models used them).
#[tokio::test]
async fn sed_append_and_insert_lines() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let appended = kernel
        .execute(r#"printf 'x\nERROR\n' | sed '/ERROR/a ---'"#)
        .await
        .expect("a should run");
    assert_eq!(appended.text_out(), "x\nERROR\n---\n", "{:?}", appended.err);

    let inserted = kernel
        .execute(r#"printf 'body\n' | sed '1i #!/bin/sh'"#)
        .await
        .expect("i should run");
    assert_eq!(inserted.text_out(), "#!/bin/sh\nbody\n", "{:?}", inserted.err);
}

/// `y///` transliterates (both models used it).
#[tokio::test]
async fn sed_transliterate() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo 'abc' | sed 'y/abc/xyz/'"#)
        .await
        .expect("y should run");
    assert_eq!(result.text_out().trim(), "xyz", "{:?}", result.err);
}

/// `-E`/`-r` are accepted no-ops (Gemini reached for `-E`). The engine is
/// already ERE, so the capture-group swap just works.
#[tokio::test]
async fn sed_dash_e_flag_is_accepted() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo 'John Smith' | sed -E 's/(\w+) (\w+)/\2 \1/'"#)
        .await
        .expect("-E should be accepted");
    assert_eq!(result.text_out().trim(), "Smith John", "{:?}", result.err);
}

/// The BRE capture-group idiom (Claude-haiku reached for `\(…\)`) is rejected
/// loudly with an ERE hint, instead of silently not matching.
#[tokio::test]
async fn sed_bre_capture_groups_rejected_with_hint() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let err = kernel
        .execute(r#"echo 'John Smith' | sed 's/\(\w*\) \(\w*\)/\2 \1/'"#)
        .await
        .expect_err("BRE groups should be rejected");
    let msg = err.to_string();
    assert!(msg.contains("E006"), "should name the code: {msg}");
    assert!(msg.contains("ERE"), "should hint ERE: {msg}");
}

/// BRE alternation `\|` and intervals `\{N\}` mean literal `|`/`{` under ERE —
/// they used to silently match the wrong thing. Now they're rejected with an ERE
/// hint, through the full kernel path.
#[tokio::test]
async fn sed_bre_alternation_and_interval_rejected_with_hint() {
    let kernel = kernel_at(tempfile::tempdir().unwrap().path());
    let err = kernel
        .execute(r#"echo 'cat' | sed 's/cat\|dog/X/'"#)
        .await
        .expect_err("BRE alternation should be rejected");
    let msg = err.to_string();
    assert!(msg.contains("E006"), "should name the code: {msg}");
    assert!(msg.contains("ERE"), "should hint ERE: {msg}");

    let err = kernel
        .execute(r#"echo 'aa' | sed 's/a\{2\}/X/'"#)
        .await
        .expect_err("BRE interval should be rejected");
    assert!(err.to_string().contains("ERE"), "interval should hint ERE: {err}");
}

/// `-e EXPR file` (flag form) reads the file, not stdin. This exercises the
/// `file_pos` fix: when `-e` supplies the expression, the first positional is
/// the file. (Pre-fix, the flags-based `file_pos` check misrouted this to
/// stdin.)
#[tokio::test]
async fn sed_e_flag_then_file_reads_the_file() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("in.txt"), "one two\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("sed -e 's/one/1/' -e 's/two/2/' in.txt")
        .await
        .expect("sed -e ... file should run");
    assert!(result.ok(), "sed should succeed: {:?}", result.err);
    assert_eq!(result.text_out().trim(), "1 2", "got: {}", result.text_out());
}
