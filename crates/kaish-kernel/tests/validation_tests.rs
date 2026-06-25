//! Integration tests for pre-execution validation.
//!
//! These tests verify that the validator correctly blocks execution
//! for scripts with Error-level issues, while allowing scripts with
//! only Warning-level issues to execute.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

#[cfg(feature = "localfs")]
use std::path::PathBuf;

use kaish_kernel::Kernel;
#[cfg(feature = "localfs")]
use kaish_kernel::KernelConfig;

/// Helper to create a transient kernel for testing.
async fn make_kernel() -> std::sync::Arc<Kernel> {
    Kernel::transient().expect("should create kernel").into_arc()
}

/// Helper to create a kernel with CWD in the repo (for tests that run external commands).
#[cfg(feature = "localfs")]
fn make_repo_kernel() -> std::sync::Arc<Kernel> {
    let config = KernelConfig::repl()
        .with_cwd(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
    Kernel::new(config).expect("should create kernel").into_arc()
}

// ============================================================================
// Tests that verify validation BLOCKS execution (Error-level issues)
// ============================================================================

#[tokio::test]
async fn validation_blocks_break_outside_loop() {
    let kernel = make_kernel().await;
    let result = kernel.execute("break").await;

    assert!(result.is_err(), "break outside loop should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("loop") || err.contains("validation"),
        "error should mention loop or validation: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_dollar_question_field_access() {
    let kernel = make_kernel().await;
    let result = kernel.execute(r#"echo "${?.data}""#).await;

    assert!(
        result.is_err(),
        "${{?.data}} should fail validation (use kaish-last instead)"
    );
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("kaish-last"),
        "error should suggest kaish-last: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_dollar_question_code_field() {
    let kernel = make_kernel().await;
    // Any non-empty field path on $? is removed — including the ones that
    // used to be valid (.code, .ok). $? alone is still POSIX-shaped.
    let result = kernel.execute(r#"echo "${?.code}""#).await;
    assert!(result.is_err(), "${{?.code}} should fail validation");
}

#[tokio::test]
async fn validation_blocks_continue_outside_loop() {
    let kernel = make_kernel().await;
    let result = kernel.execute("continue").await;

    assert!(result.is_err(), "continue outside loop should fail validation");
}

#[tokio::test]
async fn validation_blocks_return_outside_function() {
    let kernel = make_kernel().await;
    let result = kernel.execute("return").await;

    assert!(result.is_err(), "return outside function should fail validation");
}

#[tokio::test]
async fn validation_blocks_invalid_regex() {
    let kernel = make_kernel().await;
    // Unclosed bracket is invalid regex
    let result = kernel.execute("grep '[' /dev/null").await;

    assert!(result.is_err(), "invalid regex should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("regex") || err.contains("validation"),
        "error should mention regex or validation: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_seq_zero_increment() {
    let kernel = make_kernel().await;
    // seq FIRST INCREMENT LAST with increment=0 would loop forever
    let result = kernel.execute("seq 1 0 10").await;

    assert!(result.is_err(), "seq with zero increment should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("zero") || err.contains("increment") || err.contains("validation"),
        "error should mention zero/increment or validation: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_bare_var_in_for_loop() {
    let kernel = make_kernel().await;
    // `for i in $VAR` is always wrong in kaish - no implicit word splitting
    let result = kernel.execute(r#"
        ITEMS="a b c"
        for i in $ITEMS; do
            echo $i
        done
    "#).await;

    assert!(result.is_err(), "bare variable in for loop should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("word splitting") || err.contains("iterate once") || err.contains("E012"),
        "error should mention word splitting or iterate once: {}",
        err
    );
}

#[tokio::test]
async fn validation_allows_split_in_for_loop() {
    let kernel = make_kernel().await;
    // `for i in $(split "$VAR")` is the correct way
    let result = kernel.execute(r#"
        ITEMS="a b c"
        for i in $(split "$ITEMS"); do
            echo $i
        done
    "#).await;

    assert!(result.is_ok(), "split in for loop should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "split in for loop should execute successfully");
    assert!(exec.text_out().contains("a") && exec.text_out().contains("b") && exec.text_out().contains("c"));
}

#[tokio::test]
async fn validation_allows_seq_in_for_loop() {
    let kernel = make_kernel().await;
    // `for i in $(seq 1 3)` works because seq returns a JSON array
    let result = kernel.execute(r#"
        for i in $(seq 1 3); do
            echo $i
        done
    "#).await;

    assert!(result.is_ok(), "seq in for loop should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "seq in for loop should execute successfully");
}

// ============================================================================
// Tests that verify validation ALLOWS execution (Warning-level issues only)
// ============================================================================

#[tokio::test]
async fn validation_allows_unknown_command_with_warning() {
    let kernel = make_kernel().await;
    // Unknown command is a warning, not error
    // It will fail at runtime, not validation
    let result = kernel.execute("nonexistent_command_xyz").await;

    // Should get past validation but may fail at runtime
    // The key test is that it doesn't fail with "validation failed"
    match result {
        Ok(exec_result) => {
            // Got past validation, runtime failure is OK
            assert!(!exec_result.ok(), "unknown command should fail at runtime");
        }
        Err(e) => {
            let err = e.to_string();
            // Should NOT fail due to validation
            assert!(
                !err.contains("validation failed"),
                "unknown command should be warning not error: {}",
                err
            );
        }
    }
}

#[tokio::test]
async fn validation_allows_undefined_variable_with_warning() {
    let kernel = make_kernel().await;
    // Undefined variable is a warning
    let result = kernel.execute("echo $UNDEFINED_VARIABLE_XYZ").await;

    // Validation should NOT reject this — undefined vars are warnings, not errors.
    // The runtime may produce a failure ExecResult or propagate an Err for the
    // undefined variable, but that's separate from validation.
    match result {
        Ok(_exec_result) => {
            // Reached execution — validation allowed it. Runtime behavior for
            // undefined vars is a separate concern (should expand to empty, but
            // currently produces a runtime error).
        }
        Err(e) => {
            let err = e.to_string();
            assert!(
                !err.contains("validation failed"),
                "undefined variable should be warning: {}",
                err
            );
        }
    }
}

// ============================================================================
// Tests for skip_validation flag
// ============================================================================

#[tokio::test]
async fn skip_validation_allows_break_outside_loop() {
    use kaish_kernel::KernelConfig;

    let config = KernelConfig::transient().with_skip_validation(true);
    let kernel = Kernel::new(config).expect("should create kernel");

    // With validation skipped, break outside loop passes validation
    // Runtime behavior: break at top level may be ignored or cause an error
    let result = kernel.execute("break").await;

    match result {
        Ok(_) => {
            // Got past validation - this is the key assertion
            // Runtime may succeed (break ignored) or fail, either is acceptable
        }
        Err(e) => {
            let err = e.to_string();
            // Should NOT say "validation failed" since we skipped it
            assert!(
                !err.contains("validation failed"),
                "should not fail validation when skipped: {}",
                err
            );
        }
    }
}

// ============================================================================
// Tests that valid scripts pass validation
// ============================================================================

#[tokio::test]
async fn validation_passes_for_valid_script() {
    let kernel = make_kernel().await;

    // A completely valid script
    let result = kernel.execute(r#"
        x=1
        echo $x
    "#).await;

    assert!(result.is_ok(), "valid script should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "valid script should execute successfully");
}

#[tokio::test]
async fn validation_passes_for_loop_with_break() {
    let kernel = make_kernel().await;

    // break inside a loop is valid
    let result = kernel.execute(r#"
        for i in 1 2 3; do
            if [[ $i == 2 ]]; then
                break
            fi
            echo $i
        done
    "#).await;

    assert!(result.is_ok(), "break inside loop should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "loop with break should execute successfully");
}

#[tokio::test]
async fn validation_passes_for_valid_grep() {
    let kernel = make_kernel().await;

    // Valid regex pattern
    let result = kernel.execute("echo 'hello world' | grep 'hello'").await;

    assert!(result.is_ok(), "valid grep should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "valid grep should execute successfully");
}

#[tokio::test]
async fn validation_passes_for_valid_seq() {
    let kernel = make_kernel().await;

    // Non-zero increment is valid
    let result = kernel.execute("seq 1 2 10").await;

    assert!(result.is_ok(), "valid seq should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "valid seq should execute successfully");
    assert!(exec.text_out().contains("1") && exec.text_out().contains("9"));
}

// ============================================================================
// Tests for bare glob expansion (globs now parse and expand at runtime)
// ============================================================================

#[tokio::test]
async fn validation_accepts_glob_pattern_ls_star() {
    let kernel = make_kernel().await;
    // `ls *.txt` now parses and expands at runtime.
    // In an empty temp dir, it returns "no matches" error at runtime (not parse error).
    let result = kernel.execute("ls *.txt").await;

    // Parse succeeds, runtime may fail with "no matches"
    match result {
        Ok(exec) => {
            // If there happen to be .txt files, it succeeds
            assert!(exec.code == 0 || exec.code == 1);
        }
        Err(e) => {
            let err = e.to_string();
            assert!(err.contains("no matches"), "expected no-matches error: {}", err);
        }
    }
}

#[tokio::test]
async fn validation_allows_glob_pattern_ls_quoted() {
    let kernel = make_kernel().await;
    // ls now accepts glob patterns natively
    let result = kernel.execute("ls \"*.txt\"").await;

    assert!(result.is_ok(), "ls should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_bare_glob_rm_bak_parses() {
    let kernel = make_kernel().await;
    // rm *.bak now parses; runtime fails with "no matches" in empty dir
    let result = kernel.execute("rm *.bak").await;
    match result {
        Ok(_) => {} // might succeed if files exist
        Err(e) => assert!(e.to_string().contains("no matches"), "expected no-matches: {}", e),
    }
}

#[tokio::test]
async fn validation_bare_glob_question_parses() {
    let kernel = make_kernel().await;
    // file?.log now parses as a GlobPattern; fails at runtime with no matches
    let result = kernel.execute("cat file?.log").await;
    match result {
        Ok(_) => {}
        Err(e) => assert!(e.to_string().contains("no matches"), "expected no-matches: {}", e),
    }
}

#[tokio::test]
async fn validation_bare_glob_with_path_parses() {
    let kernel = make_kernel().await;
    // src/*.rs now parses as GlobPattern
    let result = kernel.execute("cp src/*.rs dest/").await;
    match result {
        Ok(_) => {}
        Err(e) => assert!(e.to_string().contains("no matches"), "expected no-matches: {}", e),
    }
}

#[tokio::test]
async fn validation_bare_glob_bracket_parses() {
    let kernel = make_kernel().await;
    // [abc].txt now parses and expands
    let result = kernel.execute("echo [abc].txt").await;
    match result {
        Ok(_) => {}
        Err(e) => assert!(e.to_string().contains("no matches"), "expected no-matches: {}", e),
    }
}

#[tokio::test]
async fn validation_allows_glob_builtin() {
    let kernel = make_kernel().await;
    // The `glob` builtin correctly takes pattern arguments
    let result = kernel.execute("glob \"*.txt\"").await;

    // Should pass validation (runtime may return empty list)
    assert!(result.is_ok(), "glob builtin should pass validation");
}

#[tokio::test]
async fn validation_allows_grep_pattern() {
    let kernel = make_kernel().await;
    // grep takes regex patterns, not file globs
    let result = kernel.execute("echo 'func_test' | grep 'func.*test'").await;

    assert!(result.is_ok(), "grep pattern should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "grep should execute successfully");
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn validation_allows_find_pattern() {
    // Use repo-scoped kernel so `find .` walks the crate dir, not $HOME
    let kernel = make_repo_kernel();
    let result = kernel.execute("find . -name \"*.rs\" -maxdepth 2").await;

    // Should pass validation (find handles its own patterns)
    assert!(result.is_ok(), "find with pattern should pass validation");
}

#[tokio::test]
async fn validation_allows_quoted_glob_pattern() {
    let kernel = make_kernel().await;
    // Quoted patterns are passed literally to commands
    let result = kernel.execute("echo \"*.txt\"").await;

    assert!(result.is_ok(), "quoted pattern should pass validation");
    let exec = result.unwrap();
    assert!(exec.ok(), "quoted pattern should execute");
    assert!(exec.text_out().contains("*.txt"), "pattern should be literal");
}

#[tokio::test]
async fn validation_allows_correct_glob_usage() {
    let kernel = make_kernel().await;
    // Correct way: use glob builtin and iterate results
    let result = kernel.execute(r#"
        for f in $(glob "*.nonexistent"); do
            echo $f
        done
    "#).await;

    assert!(result.is_ok(), "correct glob usage should pass validation");
}

// ============================================================================
// Additional glob pattern edge cases
// ============================================================================

#[tokio::test]
async fn validation_allows_glob_double_star_in_cat() {
    let kernel = make_kernel().await;
    // cat now accepts glob patterns natively
    let result = kernel.execute("cat \"**/*.rs\"").await;

    assert!(result.is_ok(), "cat should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_quoted_glob_in_mv() {
    let kernel = make_kernel().await;
    // Quoted glob stays as a literal string — no expansion
    let result = kernel.execute("mv \"*.old\" backup/").await;
    assert!(result.is_ok(), "quoted glob should parse and validate: {result:?}");
    // At runtime the literal filename `*.old` doesn't exist; the error must
    // name it verbatim — proof the quoted glob was NOT expanded (an unquoted
    // bare glob with zero matches errors before mv ever runs).
    let r = result.expect("checked above");
    assert!(!r.ok(), "mv of a nonexistent literal should fail");
    assert!(
        r.err.contains("*.old"),
        "error should name the literal *.old: err={:?}",
        r.err
    );
}

#[tokio::test]
async fn validation_allows_glob_in_head() {
    let kernel = make_kernel().await;
    // head now accepts glob patterns natively
    let result = kernel.execute("head \"config*.yaml\"").await;

    assert!(result.is_ok(), "head should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_allows_glob_in_tail() {
    let kernel = make_kernel().await;
    // tail now accepts glob patterns natively
    let result = kernel.execute("tail \"log?.txt\"").await;

    assert!(result.is_ok(), "tail should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_allows_glob_character_class_in_cat() {
    let kernel = make_kernel().await;
    // cat now accepts glob patterns natively
    let result = kernel.execute("cat \"file[a-z].txt\"").await;

    assert!(result.is_ok(), "cat should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_allows_sed_pattern() {
    let kernel = make_kernel().await;
    // sed patterns are ERE regex, not globs. `.*\.txt` is a valid ERE that
    // resembles a glob pattern (*.txt) but uses proper regex quantifier syntax.
    // The original `s/*.txt/replaced/` used a bare `*` which is invalid ERE
    // and was correctly rejected once E006 validation landed.
    let result = kernel.execute(r"echo 'test' | sed 's/.*\.txt/replaced/'").await;

    assert!(result.is_ok(), "sed pattern should pass validation");
}

#[tokio::test]
async fn validation_allows_awk_pattern() {
    let kernel = make_kernel().await;
    // awk patterns
    let result = kernel.execute("echo 'test' | awk '/.*\\.txt/ {print}'").await;

    assert!(result.is_ok(), "awk pattern should pass validation");
}

#[tokio::test]
async fn validation_allows_jq_pattern() {
    let kernel = make_kernel().await;
    // jq filter with glob-like syntax
    let result = kernel.execute("echo '{}' | jq '.files[].name'").await;

    assert!(result.is_ok(), "jq filter should pass validation");
}

#[tokio::test]
async fn validation_allows_glob_in_pipeline_first() {
    let kernel = make_kernel().await;
    // cat now accepts glob patterns, so glob in pipeline is valid
    let result = kernel.execute("cat \"*.log\" | grep error").await;

    assert!(result.is_ok(), "cat with glob in pipeline should pass validation: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_allows_glob_in_pipeline_grep() {
    let kernel = make_kernel().await;
    // Pattern in grep (second command) is fine
    let result = kernel.execute("echo 'test.txt' | grep '.*\\.txt'").await;

    assert!(result.is_ok(), "grep pattern in pipeline should pass");
}

#[tokio::test]
async fn validation_allows_glob_with_path_prefix_in_ls() {
    let kernel = make_kernel().await;
    // ls now accepts glob patterns natively
    let result = kernel.execute("ls \"/tmp/*.log\"").await;

    assert!(result.is_ok(), "ls should accept glob patterns: {:?}", result.unwrap_err());
}

#[tokio::test]
async fn validation_allows_literal_asterisk_filename() {
    let kernel = make_kernel().await;
    // A file literally named "star" without extension - not a glob
    let result = kernel.execute("cat \"notes\"").await;

    // Should pass validation (no glob chars, will fail at runtime if file doesn't exist)
    assert!(result.is_ok(), "literal filename should pass validation");
}

#[tokio::test]
async fn validation_allows_printf_pattern() {
    let kernel = make_kernel().await;
    // printf is text output
    let result = kernel.execute("printf '%s\\n' \"*.txt\"").await;

    assert!(result.is_ok(), "printf with pattern should pass validation");
}

// ============================================================================
// Scatter/gather validation tests (E014)
// ============================================================================

#[tokio::test]
async fn validation_blocks_scatter_without_gather() {
    let kernel = make_kernel().await;
    let result = kernel.execute("seq 1 3 | scatter | echo hi").await;

    assert!(result.is_err(), "scatter without gather should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("gather") || err.contains("E014"),
        "error should mention gather or E014: {}",
        err
    );
}

#[tokio::test]
async fn validation_allows_scatter_with_gather() {
    let kernel = make_kernel().await;
    // seq | scatter | echo | gather should pass validation
    let result = kernel.execute("seq 1 3 | scatter | echo \"hi\" | gather").await;

    assert!(result.is_ok(), "scatter with gather should pass validation: {:?}", result.err());
}

// ============================================================================
// Scatter/gather explicit splitting pipeline tests
// ============================================================================

#[tokio::test]
async fn scatter_seq_structured_data() {
    let kernel = make_kernel().await;
    // seq produces structured JSON array, scatter consumes it
    let result = kernel.execute(r#"seq 1 3 | scatter | echo "$ITEM" | gather"#).await;

    assert!(result.is_ok(), "seq | scatter | gather should pass: {:?}", result.err());
    let exec = result.unwrap();
    assert!(exec.ok(), "pipeline should succeed: {}", exec.err);
    assert!(exec.text_out().contains("1"), "should contain 1: {}", exec.text_out());
    assert!(exec.text_out().contains("2"), "should contain 2: {}", exec.text_out());
    assert!(exec.text_out().contains("3"), "should contain 3: {}", exec.text_out());
}

#[tokio::test]
async fn scatter_split_structured_data() {
    let kernel = make_kernel().await;
    // split produces structured JSON array, scatter consumes it
    let result = kernel.execute(r#"split "a,b,c" "," | scatter --as X | echo "got $X" | gather"#).await;

    assert!(result.is_ok(), "split | scatter | gather should pass: {:?}", result.err());
    let exec = result.unwrap();
    assert!(exec.ok(), "pipeline should succeed: {}", exec.err);
    assert!(exec.text_out().contains("got a"), "should contain 'got a': {}", exec.text_out());
    assert!(exec.text_out().contains("got b"), "should contain 'got b': {}", exec.text_out());
    assert!(exec.text_out().contains("got c"), "should contain 'got c': {}", exec.text_out());
}

#[tokio::test]
async fn scatter_split_stdin_pipe() {
    let kernel = make_kernel().await;
    // echo | split | scatter — split reads from stdin
    let result = kernel.execute(r#"echo "x,y,z" | split "," | scatter --as V | echo "got $V" | gather"#).await;

    assert!(result.is_ok(), "echo | split | scatter should pass: {:?}", result.err());
    let exec = result.unwrap();
    assert!(exec.ok(), "pipeline should succeed: {}", exec.err);
    assert!(exec.text_out().contains("got x"), "should contain 'got x': {}", exec.text_out());
    assert!(exec.text_out().contains("got y"), "should contain 'got y': {}", exec.text_out());
    assert!(exec.text_out().contains("got z"), "should contain 'got z': {}", exec.text_out());
}

#[tokio::test]
async fn scatter_single_item() {
    let kernel = make_kernel().await;
    // Single-line text (no splitting needed)
    let result = kernel.execute(r#"echo "hello" | scatter | echo "$ITEM" | gather"#).await;

    assert!(result.is_ok(), "single item scatter should pass: {:?}", result.err());
    let exec = result.unwrap();
    assert!(exec.ok(), "pipeline should succeed: {}", exec.err);
    assert!(exec.text_out().contains("hello"), "should contain 'hello': {}", exec.text_out());
}

#[tokio::test]
async fn scatter_empty_input() {
    let kernel = make_kernel().await;
    // Empty input to scatter should succeed with no output
    let result = kernel.execute(r#"split "" "," | scatter | echo "$ITEM" | gather"#).await;

    assert!(result.is_ok(), "empty scatter should pass: {:?}", result.err());
    let exec = result.unwrap();
    assert!(exec.ok(), "pipeline should succeed: {}", exec.err);
}

// ============================================================================
// Span tracking for issues raised inside heredoc bodies.
//
// Pre-fix the validator emitted None spans for issues found inside heredoc
// interpolations — `format(source)` produced a bare message with no location.
// With the SpannedPart flow added by the heredoc work, body-internal issues
// now carry byte offsets in the original source, and `format(source)`
// renders `line:col [code]: msg\n  | source-line`.
// ============================================================================

#[tokio::test]
async fn validation_issue_in_heredoc_body_carries_span() {
    use std::collections::HashMap;
    use kaish_kernel::parser::parse;
    use kaish_kernel::tools::{register_builtins, ToolRegistry};
    use kaish_kernel::validator::Validator;

    let source = "cat <<EOF\n${UNDEFINED_VAR}\nEOF";
    let program = parse(source).expect("source parses");

    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let user_tools = HashMap::new();
    let validator = Validator::new(&registry, &user_tools);
    let issues = validator.validate(&program);

    // The body references UNDEFINED_VAR — validator should warn about it
    // and the warning should now carry a span (was always None pre-fix).
    let undef = issues
        .iter()
        .find(|i| i.message.contains("UNDEFINED_VAR"))
        .expect("should warn about UNDEFINED_VAR");
    let span = undef.span.expect("span must be populated for body-internal issue");

    // "cat <<EOF\n" is 10 bytes; "${UNDEFINED_VAR}" lives on line 2 col 1.
    let (line, col) = span.to_line_col(source);
    assert_eq!(line, 2, "body-internal issue should report line 2");
    assert_eq!(col, 1, "issue should start at column 1 of body line");

    // format(source) must render line:col + the offending source line.
    let rendered = undef.format(source);
    assert!(rendered.starts_with("2:1"), "rendered should start with line:col, got: {rendered}");
    assert!(
        rendered.contains("UNDEFINED_VAR"),
        "rendered should mention the variable: {rendered}",
    );
    assert!(
        rendered.contains("${UNDEFINED_VAR}"),
        "rendered should include the source-line caret showing the offending line: {rendered}",
    );
}

#[tokio::test]
async fn validation_issue_in_double_quoted_string_still_works() {
    // Sibling check: spanless interpolation (regular double-quoted strings)
    // still produces issues, just without spans. This pins the asymmetry —
    // universal spanning is a follow-up refactor.
    use std::collections::HashMap;
    use kaish_kernel::parser::parse;
    use kaish_kernel::tools::{register_builtins, ToolRegistry};
    use kaish_kernel::validator::Validator;

    let source = r#"echo "value is ${UNDEFINED_VAR_TWO}""#;
    let program = parse(source).expect("source parses");

    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let user_tools = HashMap::new();
    let validator = Validator::new(&registry, &user_tools);
    let issues = validator.validate(&program);

    let undef = issues
        .iter()
        .find(|i| i.message.contains("UNDEFINED_VAR_TWO"))
        .expect("should still warn about double-quoted-string undefs");
    // Spanless path — span is None until universal spanning lands.
    assert!(
        undef.span.is_none(),
        "double-quoted strings remain spanless until follow-up refactor",
    );
}

#[tokio::test]
async fn validation_issue_in_heredoc_body_full_rendering_snapshot() {
    // Snapshot the full ValidationIssue::format(source) output for a
    // body-internal warning. Locks the user-visible diagnostic layout —
    // the `line:col [code]: msg\n  | source-line` shape. If the format
    // changes (e.g., adding colours, switching to ariadne), this test
    // will alert and the snapshot can be reviewed with `cargo insta`.
    use std::collections::HashMap;
    use kaish_kernel::parser::parse;
    use kaish_kernel::tools::{register_builtins, ToolRegistry};
    use kaish_kernel::validator::Validator;

    let source = "cat <<EOF\n${STILL_UNDEFINED}\nEOF";
    let program = parse(source).expect("source parses");
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let user_tools = HashMap::new();
    let validator = Validator::new(&registry, &user_tools);
    let issues = validator.validate(&program);

    let undef = issues
        .iter()
        .find(|i| i.message.contains("STILL_UNDEFINED"))
        .expect("expected undefined-variable warning");

    insta::assert_snapshot!(undef.format(source));
}

// ============================================================================
// Tests that verify selective warning SURFACING (W006 reaches the agent;
// the generic undefined-command warning stays trace-only)
// ============================================================================

#[tokio::test]
async fn posix_test_command_surfaces_advisory_but_still_runs() {
    let kernel = make_kernel().await;
    // `test` is not a kaish command; the validator advisory (W006) must reach
    // the agent on stderr, and the warning must NOT block execution.
    let result = kernel
        .execute("test -n hi")
        .await
        .expect("a warning must not turn into a validation error");
    assert!(
        result.err.contains("W006") && result.err.contains("[["),
        "the W006 `[[ … ]]` advisory should surface on stderr, got err: {:?}",
        result.err
    );
}

#[tokio::test]
async fn generic_undefined_command_warning_stays_trace_only() {
    let kernel = make_kernel().await;
    // An ordinary unknown command warns at validation, but that warning is
    // trace-only — it must not be surfaced to stderr (otherwise every external
    // command would spam it). Only the runtime "command not found" may appear.
    let result = kernel
        .execute("frobnicate_xyz_not_a_real_command")
        .await
        .expect("an undefined-command warning must not block execution");
    assert!(
        !result.err.contains("W006") && !result.err.contains("builtin registry"),
        "the generic undefined-command validation warning must not surface, got err: {:?}",
        result.err
    );
}
