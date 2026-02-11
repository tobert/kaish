//! Shell variable and expression handling bug tests.
//!
//! These tests were written to catch bugs discovered during real-world testing.
//! Tests are written first (TDD style), then code is fixed to make them pass.

use kaish_kernel::Kernel;

// ============================================================================
// Bug 1: ${?} braced form returns 0 instead of actual exit code
// ============================================================================

#[tokio::test]
async fn test_braced_last_exit_code_after_success() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("true; echo ${?}").await.unwrap();
    assert_eq!(result.out.trim(), "0", "Expected 0 after true command");
}

#[tokio::test]
async fn test_braced_last_exit_code_after_failure() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("false; echo ${?}").await.unwrap();
    assert_eq!(result.out.trim(), "1", "Expected 1 after false command");
}

#[tokio::test]
async fn test_braced_vs_unbraced_exit_code_equivalence() {
    let kernel = Kernel::transient().unwrap();
    // Both forms should give the same result
    let result1 = kernel.execute("false; echo $?").await.unwrap();
    let result2 = kernel.execute("false; echo ${?}").await.unwrap();
    assert_eq!(
        result1.out.trim(),
        result2.out.trim(),
        "Braced and unbraced $? should be equivalent"
    );
}

// ============================================================================
// Bug 2: $? in arithmetic $(($? + 1)) fails
// ============================================================================

#[tokio::test]
async fn test_exit_code_in_arithmetic() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("false; echo $(( $? + 10 ))").await.unwrap();
    assert_eq!(result.out.trim(), "11", "Expected 1 + 10 = 11");
}

#[tokio::test]
async fn test_exit_code_in_arithmetic_after_success() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("true; echo $(( $? * 5 ))").await.unwrap();
    assert_eq!(result.out.trim(), "0", "Expected 0 * 5 = 0");
}

#[tokio::test]
async fn test_pid_in_arithmetic() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo $(( $$ % 100000 ))").await.unwrap();
    // Just verify it parses and returns a number
    let val: i64 = result.out.trim().parse().expect("Should be a number");
    assert!(val >= 0, "PID mod should be non-negative");
}

#[tokio::test]
async fn test_braced_exit_code_in_arithmetic() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("false; echo $(( ${?} + 5 ))").await.unwrap();
    assert_eq!(result.out.trim(), "6", "Expected 1 + 5 = 6");
}

// ============================================================================
// Bug 3: return N leaks value to stdout
// ============================================================================

#[tokio::test]
async fn test_return_does_not_output_value() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
f() {
    echo "output"
    return 5
}
X=$(f)
echo "captured: [$X]"
"#,
        )
        .await
        .unwrap();
    // The captured output should only contain "output", not the return value
    assert!(
        result.out.contains("captured: [output]"),
        "Expected 'captured: [output]', got: {}",
        result.out
    );
    // Should NOT contain JSON or the number 5 in the captured var
    assert!(
        !result.out.contains("captured: [5"),
        "Return value leaked to stdout: {}",
        result.out
    );
}

#[tokio::test]
async fn test_return_sets_exit_code() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
f() { return 42; }
f
echo $?
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("42"),
        "Expected exit code 42, got: {}",
        result.out
    );
}

#[tokio::test]
async fn test_return_without_value() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
f() { echo "hi"; return; }
X=$(f)
echo "got: [$X]"
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("got: [hi]"),
        "Expected 'got: [hi]', got: {}",
        result.out
    );
}

// ============================================================================
// Bug 4: local keyword doesn't scope variables
// ============================================================================

#[tokio::test]
async fn test_local_variable_scoping() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
x=outer
f() {
    local x = inner
    echo "in func: $x"
}
f
echo "after func: $x"
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("in func: inner"),
        "Local var should be 'inner' inside function: {}",
        result.out
    );
    assert!(
        result.out.contains("after func: outer"),
        "Outer var should be 'outer' after function: {}",
        result.out
    );
}

#[tokio::test]
async fn test_local_does_not_affect_outer_scope() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
count=0
increment() {
    local count = 99
}
increment
echo $count
"#,
        )
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "0",
        "Outer 'count' should still be 0: {}",
        result.out
    );
}

#[tokio::test]
async fn test_non_local_modifies_outer_scope() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
count=0
increment() {
    count=99
}
increment
echo $count
"#,
        )
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "99",
        "Without local, 'count' should be modified: {}",
        result.out
    );
}

// ============================================================================
// Bug 5: Nested command substitution $(echo $(echo x)) fails
// ============================================================================

#[tokio::test]
async fn test_nested_command_substitution() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo $(echo $(echo hello))").await.unwrap();
    assert_eq!(
        result.out.trim(),
        "hello",
        "Nested cmd subst should work: {}",
        result.out
    );
}

#[tokio::test]
async fn test_deeply_nested_command_substitution() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("echo $(echo $(echo $(echo deep)))")
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "deep",
        "Deeply nested cmd subst should work: {}",
        result.out
    );
}

// ============================================================================
// Behavior: No implicit word splitting in for loops
// ============================================================================
// kaish does NOT split strings on whitespace. Use `split` for explicit splitting
// or use builtins that return arrays (seq, glob, find).

#[tokio::test]
async fn test_command_subst_no_implicit_split() {
    // In kaish, $(echo "a b c") returns ONE string, not three words
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
for x in $(echo "a b c"); do
    echo "item: $x"
done
"#,
        )
        .await
        .unwrap();
    // Should iterate ONCE with the whole string
    assert!(
        result.out.contains("item: a b c"),
        "Should have whole string: {}",
        result.out
    );
    // Should NOT have separate items
    assert!(
        !result.out.contains("item: a\n"),
        "Should NOT split: {}",
        result.out
    );
}

#[tokio::test]
async fn test_command_subst_with_explicit_split() {
    // Use split for explicit word splitting
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
for x in $(split "a b c"); do
    echo "item: $x"
done
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("item: a"),
        "Should have item a: {}",
        result.out
    );
    assert!(
        result.out.contains("item: b"),
        "Should have item b: {}",
        result.out
    );
    assert!(
        result.out.contains("item: c"),
        "Should have item c: {}",
        result.out
    );
}

#[tokio::test]
async fn test_for_loop_with_seq_returns_array() {
    // seq returns a JSON array, which iterates properly
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
for num in $(seq 1 3); do
    echo "number $num"
done
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("number 1"),
        "Should have number 1: {}",
        result.out
    );
    assert!(
        result.out.contains("number 2"),
        "Should have number 2: {}",
        result.out
    );
    assert!(
        result.out.contains("number 3"),
        "Should have number 3: {}",
        result.out
    );
}

// ============================================================================
// Bug 7: >&2 and 1>&2 redirects don't parse
// ============================================================================

#[tokio::test]
async fn test_stdout_to_stderr_redirect_1_ampersand_2() {
    let kernel = Kernel::transient().unwrap();
    // This should parse and execute without error
    let result = kernel.execute("echo error 1>&2").await.unwrap();
    // Output should go to stderr, not stdout
    assert!(
        result.err.contains("error"),
        "Expected 'error' in stderr: stdout={}, stderr={}",
        result.out,
        result.err
    );
}

#[tokio::test]
async fn test_stdout_to_stderr_redirect_ampersand_2() {
    let kernel = Kernel::transient().unwrap();
    // Shorthand form: >&2 is equivalent to 1>&2
    let result = kernel.execute("echo warning >&2").await.unwrap();
    assert!(
        result.err.contains("warning"),
        "Expected 'warning' in stderr: stdout={}, stderr={}",
        result.out,
        result.err
    );
}

// ============================================================================
// Bug 8: ${$} braced PID form (same issue as ${?})
// ============================================================================

#[tokio::test]
async fn test_braced_current_pid() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo ${$}").await.unwrap();
    // Should be a positive integer (the PID)
    let pid: u32 = result
        .out
        .trim()
        .parse()
        .expect("${$} should be a number");
    assert!(pid > 0, "PID should be positive: {}", pid);
}

#[tokio::test]
async fn test_braced_vs_unbraced_pid_equivalence() {
    let kernel = Kernel::transient().unwrap();
    let result1 = kernel.execute("echo $$").await.unwrap();
    let result2 = kernel.execute("echo ${$}").await.unwrap();
    assert_eq!(
        result1.out.trim(),
        result2.out.trim(),
        "Braced and unbraced $$ should be equivalent"
    );
}

// ============================================================================
// Additional edge cases and combinations
// ============================================================================

#[tokio::test]
async fn test_exit_code_in_string_interpolation() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"false; echo "exit code: $?""#)
        .await
        .unwrap();
    assert!(
        result.out.contains("exit code: 1"),
        "Expected 'exit code: 1': {}",
        result.out
    );
}

#[tokio::test]
async fn test_braced_exit_code_in_string_interpolation() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"false; echo "exit code: ${?}""#)
        .await
        .unwrap();
    assert!(
        result.out.contains("exit code: 1"),
        "Expected 'exit code: 1': {}",
        result.out
    );
}

// ============================================================================
// Bug 9: $(true) and $(false) fail to parse - builtins in command substitution
// ============================================================================

#[tokio::test]
async fn test_cmd_subst_with_true() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("VAR=$(true); echo \"exit: $?\"").await.unwrap();
    assert!(
        result.out.contains("exit: 0"),
        "$(true) should succeed with exit 0: {}",
        result.out
    );
}

#[tokio::test]
async fn test_cmd_subst_with_false() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("VAR=$(false); echo \"exit: $?\"").await.unwrap();
    assert!(
        result.out.contains("exit: 1"),
        "$(false) should fail with exit 1: {}",
        result.out
    );
}

// Note: `if $(true); then` is NOT valid kaish syntax because $(true)
// evaluates to empty string which isn't a command. Use `if true; then` instead.

// ============================================================================
// Bug 10: export VAR="value" doesn't set the variable
// ============================================================================

#[tokio::test]
async fn test_export_with_value() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("export FOO=\"bar\"; echo $FOO").await.unwrap();
    assert_eq!(
        result.out.trim(),
        "bar",
        "export should set variable value: {}",
        result.out
    );
}

#[tokio::test]
async fn test_export_multiple_vars() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("export A=1; export B=2; echo \"$A $B\"").await.unwrap();
    assert_eq!(
        result.out.trim(),
        "1 2",
        "export should set multiple variables: {}",
        result.out
    );
}

// ============================================================================
// Bug 11: Positional params in arithmetic $(($1 + $2)) always return 0
// ============================================================================

#[tokio::test]
async fn test_positional_params_in_arithmetic() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
add() {
    echo $(($1 + $2))
}
add 3 4
"#,
        )
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "7",
        "$(($1 + $2)) with 3 4 should be 7: {}",
        result.out
    );
}

#[tokio::test]
async fn test_positional_params_arithmetic_multiply() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
mul() {
    echo $(($1 * $2))
}
mul 5 6
"#,
        )
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "30",
        "$(($1 * $2)) with 5 6 should be 30: {}",
        result.out
    );
}

#[tokio::test]
async fn test_positional_params_arithmetic_with_variable() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
calc() {
    local base = 10
    echo $(($base + $1))
}
calc 5
"#,
        )
        .await
        .unwrap();
    assert_eq!(
        result.out.trim(),
        "15",
        "$(($base + $1)) with base=10 and $1=5 should be 15: {}",
        result.out
    );
}

#[tokio::test]
async fn test_local_with_command_substitution() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
val=original
f() {
    local val = $(echo "from_cmd")
    echo "local: $val"
}
f
echo "outer: $val"
"#,
        )
        .await
        .unwrap();
    assert!(
        result.out.contains("local: from_cmd"),
        "Local with cmd subst: {}",
        result.out
    );
    assert!(
        result.out.contains("outer: original"),
        "Outer unchanged: {}",
        result.out
    );
}

// ============================================================================
// Bug 12: Nested ${VAR:-default} doesn't work
// ============================================================================

#[tokio::test]
async fn test_nested_var_default() {
    let kernel = Kernel::transient().unwrap();
    // All variables unset, should return the innermost default "deep"
    let result = kernel.execute(r#"echo "${A:-${B:-deep}}""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "deep",
        "Nested defaults should work: {}",
        result.out
    );
}

#[tokio::test]
async fn test_nested_var_default_outer_set() {
    let kernel = Kernel::transient().unwrap();
    // A is set, should return A's value
    let result = kernel.execute(r#"A=outer; echo "${A:-${B:-deep}}""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "outer",
        "Outer var set should return outer: {}",
        result.out
    );
}

#[tokio::test]
async fn test_nested_var_default_middle_set() {
    let kernel = Kernel::transient().unwrap();
    // A unset, B set - should return B's value
    let result = kernel.execute(r#"B=middle; echo "${A:-${B:-deep}}""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "middle",
        "Middle var set should return middle: {}",
        result.out
    );
}

#[tokio::test]
async fn test_deeply_nested_defaults() {
    let kernel = Kernel::transient().unwrap();
    // Three levels of nesting
    let result = kernel.execute(r#"echo "${A:-${B:-${C:-deepest}}}""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "deepest",
        "Deeply nested defaults should work: {}",
        result.out
    );
}

// ============================================================================
// Bug 13: Command substitution in strings "$(cmd)" doesn't work
// ============================================================================

#[tokio::test]
async fn test_cmd_subst_in_string() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"echo "inner: $(echo nested)""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "inner: nested",
        "Command subst in string should work: {}",
        result.out
    );
}

#[tokio::test]
async fn test_cmd_subst_in_string_with_var() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"VAL=world; echo "hello $(echo $VAL)""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "hello world",
        "Command subst with var in string should work: {}",
        result.out
    );
}

#[tokio::test]
async fn test_var_in_default_with_cmd_subst() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"echo "${UNSET:-$(echo computed)}""#).await.unwrap();
    assert_eq!(
        result.out.trim(),
        "computed",
        "Command subst in default value should work: {}",
        result.out
    );
}

// ============================================================================
// COMPOUND TEST EXPRESSIONS: [[ A && B ]], [[ A || B ]], [[ ! A ]]
// ============================================================================

#[tokio::test]
async fn test_compound_and_both_true() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ -d /tmp && -d /home ]] && echo "both""#).await.unwrap();
    assert_eq!(result.out.trim(), "both", "AND with both true should pass: {}", result.out);
}

#[tokio::test]
async fn test_compound_and_one_false() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ -d /tmp && -f /nonexistent ]] && echo "both" || echo "failed""#).await.unwrap();
    assert_eq!(result.out.trim(), "failed", "AND with one false should fail: {}", result.out);
}

#[tokio::test]
async fn test_compound_or_first_true() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ -d /tmp || -f /nonexistent ]] && echo "one""#).await.unwrap();
    assert_eq!(result.out.trim(), "one", "OR with first true should pass: {}", result.out);
}

#[tokio::test]
async fn test_compound_or_second_true() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ -f /nonexistent || -d /tmp ]] && echo "one""#).await.unwrap();
    assert_eq!(result.out.trim(), "one", "OR with second true should pass: {}", result.out);
}

#[tokio::test]
async fn test_compound_or_both_false() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ -f /nonexistent || -f /also_nonexistent ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "no", "OR with both false should fail: {}", result.out);
}

#[tokio::test]
async fn test_compound_not_true() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ ! -f /nonexistent ]] && echo "correct""#).await.unwrap();
    assert_eq!(result.out.trim(), "correct", "NOT on false should be true: {}", result.out);
}

#[tokio::test]
async fn test_compound_not_false() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ ! -d /tmp ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "no", "NOT on true should be false: {}", result.out);
}

#[tokio::test]
async fn test_compound_double_not() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ ! ! -d /tmp ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "yes", "Double NOT should cancel out: {}", result.out);
}

#[tokio::test]
async fn test_compound_not_with_and() {
    let kernel = Kernel::transient().unwrap();
    // ! binds tighter than &&, so this is: (! -f /nonexistent) && (-d /tmp)
    let result = kernel.execute(r#"[[ ! -f /nonexistent && -d /tmp ]] && echo "both""#).await.unwrap();
    assert_eq!(result.out.trim(), "both", "NOT with AND should work: {}", result.out);
}

#[tokio::test]
async fn test_compound_string_tests() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"VAR=""; [[ -z "$VAR" || -n "default" ]] && echo "ok""#).await.unwrap();
    assert_eq!(result.out.trim(), "ok", "String tests with OR should work: {}", result.out);
}

#[tokio::test]
async fn test_compound_with_comparison() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"X=5; [[ $X -gt 3 && $X -lt 10 ]] && echo "in range""#).await.unwrap();
    assert_eq!(result.out.trim(), "in range", "Comparisons with AND should work: {}", result.out);
}

#[tokio::test]
async fn test_compound_in_if() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"
        X=5
        if [[ $X -gt 0 && $X -lt 10 ]]; then
            echo "valid"
        else
            echo "invalid"
        fi
    "#).await.unwrap();
    assert_eq!(result.out.trim(), "valid", "Compound test in if should work: {}", result.out);
}

#[tokio::test]
async fn test_compound_precedence() {
    let kernel = Kernel::transient().unwrap();
    // Precedence: || has lower precedence than &&
    // [[ -f /x || -d /tmp && -d /home ]] = [[ -f /x || (-d /tmp && -d /home) ]]
    // false || (true && true) = true
    let result = kernel.execute(r#"[[ -f /nonexistent || -d /tmp && -d /home ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "yes", "Precedence: && binds tighter than ||: {}", result.out);
}

#[tokio::test]
async fn test_compound_short_circuit_and() {
    let kernel = Kernel::transient().unwrap();
    // If first fails, second shouldn't be evaluated (would error on /x)
    let result = kernel.execute(r#"[[ -f /nonexistent && $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "no", "AND should short-circuit: {}", result.out);
}

#[tokio::test]
async fn test_compound_short_circuit_or() {
    let kernel = Kernel::transient().unwrap();
    // If first succeeds, second shouldn't be evaluated
    let result = kernel.execute(r#"[[ -d /tmp || $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#).await.unwrap();
    assert_eq!(result.out.trim(), "yes", "OR should short-circuit: {}", result.out);
}

// ============================================================================
// Bug: Short flags with values (-n 5) don't consume the next positional
//
// `head -n 5` and `tail -n 5` should work like `head --lines=5`.
// Currently, build_tool_args treats short flags as always boolean,
// never looking up the schema to check if the flag takes a value.
// ============================================================================

#[tokio::test]
async fn test_short_flag_with_value_head() {
    let kernel = Kernel::transient().unwrap();
    // Write a test file
    kernel.execute(r#"printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" > /tmp/lines.txt"#).await.unwrap();

    // head -n 3 should return first 3 lines
    let result = kernel.execute("head -n 3 /tmp/lines.txt").await.unwrap();
    assert!(result.ok(), "head -n 3 should succeed: err={}", result.err);
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"], "head -n 3 should return first 3 lines, got: {:?}", lines);
}

#[tokio::test]
async fn test_short_flag_with_value_tail() {
    let kernel = Kernel::transient().unwrap();
    kernel.execute(r#"printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" > /tmp/lines.txt"#).await.unwrap();

    // tail -n 3 should return last 3 lines
    let result = kernel.execute("tail -n 3 /tmp/lines.txt").await.unwrap();
    assert!(result.ok(), "tail -n 3 should succeed: err={}", result.err);
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert_eq!(lines, vec!["8", "9", "10"], "tail -n 3 should return last 3 lines, got: {:?}", lines);
}

#[tokio::test]
async fn test_short_flag_with_value_in_pipeline() {
    let kernel = Kernel::transient().unwrap();
    // Piped input: echo | head -n 3
    let result = kernel.execute(r#"printf "a\nb\nc\nd\ne\n" | head -n 3"#).await.unwrap();
    assert!(result.ok(), "pipeline head -n 3 should succeed: err={}", result.err);
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert_eq!(lines, vec!["a", "b", "c"], "head -n 3 in pipeline should return first 3 lines, got: {:?}", lines);
}

// ============================================================================
// Bug 14: Command substitution leaks scope and cwd
//
// Side effects inside $() (cd, variable assignments) must not leak to the parent.
// Only the captured stdout escapes. Uses functions to test multi-statement
// logic since kaish only supports pipelines inside $().
// ============================================================================

#[tokio::test]
async fn test_cmd_subst_cwd_isolation() {
    let kernel = Kernel::transient().unwrap();
    // Define a function that changes cwd and prints it.
    // The captured output should be "/" but pwd after should be the original cwd.
    let result = kernel
        .execute(
            r#"
go_root() { cd /; pwd; }
X=$(go_root)
pwd
"#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "Should succeed: err={}", result.err);
    // pwd should NOT be "/", it should be the original working dir
    let pwd_output = result.out.trim();
    assert_ne!(
        pwd_output, "/",
        "CWD should not leak from command substitution, got: {}",
        pwd_output
    );
}

#[tokio::test]
async fn test_cmd_subst_variable_isolation() {
    let kernel = Kernel::transient().unwrap();
    // Variable set inside $() via a function should not leak to parent
    let result = kernel
        .execute(
            r#"
X=outer
set_inner() { X=inner; echo $X; }
Y=$(set_inner)
echo $X
"#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "Should succeed: err={}", result.err);
    assert_eq!(
        result.out.trim(),
        "outer",
        "Variable should not leak from command substitution: {}",
        result.out
    );
}

#[tokio::test]
async fn test_cmd_subst_in_string_cwd_isolation() {
    let kernel = Kernel::transient().unwrap();
    // Command substitution in string interpolation should also be isolated
    let result = kernel
        .execute(
            r#"
go_root() { cd /; pwd; }
echo "dir: $(go_root)"
pwd
"#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "Should succeed: err={}", result.err);
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert!(lines.len() >= 2, "Expected at least 2 lines: {:?}", lines);
    assert!(
        lines[0].contains("dir: /"),
        "Captured output should contain '/': {}",
        lines[0]
    );
    // The second line (pwd) should NOT be "/"
    assert_ne!(
        lines[1], "/",
        "CWD should not leak from string command substitution: {}",
        lines[1]
    );
}

#[tokio::test]
async fn test_cmd_subst_captures_output_correctly() {
    let kernel = Kernel::transient().unwrap();
    // Ensure the captured value is still correct despite isolation
    let result = kernel
        .execute(
            r#"
go_root() { cd /; pwd; }
X=$(go_root)
echo "captured: $X"
"#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "Should succeed: err={}", result.err);
    assert!(
        result.out.contains("captured: /"),
        "Should capture '/' from subshell: {}",
        result.out
    );
}
