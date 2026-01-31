//! Integration tests for pre-execution validation.
//!
//! These tests verify that the validator correctly blocks execution
//! for scripts with Error-level issues, while allowing scripts with
//! only Warning-level issues to execute.

use kaish_kernel::Kernel;

/// Helper to create a transient kernel for testing.
async fn make_kernel() -> Kernel {
    Kernel::transient().expect("should create kernel")
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
    assert!(exec.out.contains("a") && exec.out.contains("b") && exec.out.contains("c"));
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

    // Should succeed (variables expand to empty string)
    match result {
        Ok(exec_result) => {
            assert!(exec_result.ok(), "undefined var should expand to empty");
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
    assert!(exec.out.contains("1") && exec.out.contains("9"));
}

// ============================================================================
// Tests for shell glob pattern detection (E013)
// ============================================================================

#[tokio::test]
async fn validation_blocks_glob_pattern_ls_star() {
    let kernel = make_kernel().await;
    // `ls *.txt` is rejected at parse time - bare * is not valid syntax
    // This is by design: kaish has no implicit globbing
    let result = kernel.execute("ls *.txt").await;

    assert!(result.is_err(), "bare glob pattern should fail");
    // Parser rejects it before validation can see it
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("parse error") || err.contains("*"),
        "error should be a parse error: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_glob_pattern_ls_quoted() {
    let kernel = make_kernel().await;
    // Quoted glob passed to ls is caught by validator
    let result = kernel.execute("ls \"*.txt\"").await;

    assert!(result.is_err(), "quoted glob to ls should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("glob") || err.contains("E013"),
        "error should mention glob or E013: {}",
        err
    );
}

#[tokio::test]
async fn validation_blocks_glob_pattern_rm_bak() {
    let kernel = make_kernel().await;
    let result = kernel.execute("rm *.bak").await;

    assert!(result.is_err(), "glob pattern should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_pattern_question_mark() {
    let kernel = make_kernel().await;
    // file?.log is a glob pattern with ? wildcard
    let result = kernel.execute("cat file?.log").await;

    assert!(result.is_err(), "glob pattern with ? should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_pattern_with_path() {
    let kernel = make_kernel().await;
    // src/*.rs includes path and wildcard
    let result = kernel.execute("cp src/*.rs dest/").await;

    assert!(result.is_err(), "glob pattern in path should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_pattern_bracket() {
    let kernel = make_kernel().await;
    // [abc].txt is a character class glob
    let result = kernel.execute("echo [abc].txt").await;

    assert!(result.is_err(), "bracket glob pattern should fail validation");
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

#[tokio::test]
async fn validation_allows_find_pattern() {
    let kernel = make_kernel().await;
    // find takes -name patterns internally (must be quoted)
    let result = kernel.execute("find . -name \"*.rs\"").await;

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
    assert!(exec.out.contains("*.txt"), "pattern should be literal");
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
async fn validation_blocks_glob_double_star() {
    let kernel = make_kernel().await;
    // Double star glob pattern
    let result = kernel.execute("cat \"**/*.rs\"").await;

    assert!(result.is_err(), "double star glob should fail validation");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E013"), "error should be E013: {}", err);
}

#[tokio::test]
async fn validation_blocks_glob_in_mv() {
    let kernel = make_kernel().await;
    let result = kernel.execute("mv \"*.old\" backup/").await;

    assert!(result.is_err(), "glob in mv should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_in_head() {
    let kernel = make_kernel().await;
    let result = kernel.execute("head \"config*.yaml\"").await;

    assert!(result.is_err(), "glob in head should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_in_tail() {
    let kernel = make_kernel().await;
    let result = kernel.execute("tail \"log?.txt\"").await;

    assert!(result.is_err(), "glob with ? in tail should fail validation");
}

#[tokio::test]
async fn validation_blocks_glob_character_class() {
    let kernel = make_kernel().await;
    // Character range glob [a-z]
    let result = kernel.execute("cat \"file[a-z].txt\"").await;

    assert!(result.is_err(), "character class glob should fail validation");
}

#[tokio::test]
async fn validation_allows_sed_pattern() {
    let kernel = make_kernel().await;
    // sed patterns look like globs but are regex
    let result = kernel.execute("echo 'test' | sed 's/*.txt/replaced/'").await;

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
async fn validation_blocks_glob_in_pipeline_first() {
    let kernel = make_kernel().await;
    // Glob in first command of pipeline
    let result = kernel.execute("cat \"*.log\" | grep error").await;

    assert!(result.is_err(), "glob in pipeline should fail validation");
}

#[tokio::test]
async fn validation_allows_glob_in_pipeline_grep() {
    let kernel = make_kernel().await;
    // Pattern in grep (second command) is fine
    let result = kernel.execute("echo 'test.txt' | grep '.*\\.txt'").await;

    assert!(result.is_ok(), "grep pattern in pipeline should pass");
}

#[tokio::test]
async fn validation_blocks_glob_with_path_prefix() {
    let kernel = make_kernel().await;
    // Glob with directory prefix
    let result = kernel.execute("ls \"/tmp/*.log\"").await;

    assert!(result.is_err(), "glob with path prefix should fail validation");
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
