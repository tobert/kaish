//! Compat tests that run the same script through kaish AND bash, so we can
//! spot where the two diverge.
//!
//! Each `shell_compat!` invocation generates a submodule with two tests:
//!   `<name>::kaish` — always runs, executes the script via the kaish kernel.
//!   `<name>::bash`  — runs only when `KAISH_BASH_COMPAT=1` is set, executes
//!                     the script via `bash -c` and applies the matching
//!                     assertions to bash's stdout/exit code.
//!
//! Usage:
//!   cargo test --test shell_compat_tests                     # kaish only
//!   KAISH_BASH_COMPAT=1 cargo test --test shell_compat_tests # both sides
//!
//! `cargo test bash` filters to just the bash side and surfaces divergences.
//!
//! Available clauses (mix and match in any order):
//!   eq: "..."         — both sides: trimmed stdout equals the literal
//!   kaish_eq: "..."   — kaish-only stdout assertion (pair with bash_eq:
//!                       to capture an intended divergence)
//!   bash_eq: "..."    — bash-only stdout assertion
//!   contains: "..."   — both sides: stdout contains the substring
//!   absent: "..."     — both sides: stdout does not contain the substring
//!   exit: N           — both sides: process exit code equals N
//!
//! Skipped from this suite (kaish-only by design — no benefit comparing):
//!   - `local x = inner` (kaish syntax with spaces; bash uses `local x=inner`)
//!   - `--json` output, kaish-only builtins (jq, glob, write, kaish-trash)
//!   - VFS paths (`/v/...`)
//!   - find / split / seq structured-iteration behavior

mod common;

// ---- $? and ${?}: exit-code variable forms --------------------------------

shell_compat! {
    name: braced_last_exit_code_after_success,
    script: "true; echo ${?}",
    eq: "0",
}

shell_compat! {
    name: braced_last_exit_code_after_failure,
    script: "false; echo ${?}",
    eq: "1",
}

shell_compat! {
    name: exit_code_in_arithmetic,
    script: "false; echo $(( $? + 10 ))",
    eq: "11",
}

shell_compat! {
    name: exit_code_in_arithmetic_after_success,
    script: "true; echo $(( $? * 5 ))",
    eq: "0",
}

shell_compat! {
    name: braced_exit_code_in_arithmetic,
    script: "false; echo $(( ${?} + 5 ))",
    eq: "6",
}

shell_compat! {
    name: exit_code_in_string_interpolation,
    script: r#"false; echo "exit code: $?""#,
    eq: "exit code: 1",
}

shell_compat! {
    name: braced_exit_code_in_string_interpolation,
    script: r#"false; echo "exit code: ${?}""#,
    eq: "exit code: 1",
}

// ---- Command substitution -------------------------------------------------

shell_compat! {
    name: nested_command_substitution,
    script: "echo $(echo $(echo hello))",
    eq: "hello",
}

shell_compat! {
    name: deeply_nested_command_substitution,
    script: "echo $(echo $(echo $(echo deep)))",
    eq: "deep",
}

shell_compat! {
    name: cmd_subst_with_true,
    script: "VAR=$(true); echo \"exit: $?\"",
    eq: "exit: 0",
}

shell_compat! {
    name: cmd_subst_with_false,
    script: "VAR=$(false); echo \"exit: $?\"",
    eq: "exit: 1",
}

shell_compat! {
    name: cmd_subst_in_string,
    script: r#"echo "inner: $(echo nested)""#,
    eq: "inner: nested",
}

shell_compat! {
    name: cmd_subst_in_string_with_var,
    script: r#"VAL=world; echo "hello $(echo $VAL)""#,
    eq: "hello world",
}

shell_compat! {
    name: var_in_default_with_cmd_subst,
    script: r#"echo "${UNSET:-$(echo computed)}""#,
    eq: "computed",
}

// ---- Functions: return / exit-code propagation ----------------------------

shell_compat! {
    name: return_does_not_leak_to_capture,
    script: "f() {\n    echo output\n    return 5\n}\nX=$(f)\necho \"captured: [$X]\"\n",
    contains: "captured: [output]",
    absent: "captured: [5",
}

shell_compat! {
    name: return_sets_exit_code,
    script: "f() { return 42; }\nf\necho $?\n",
    eq: "42",
}

shell_compat! {
    name: cmd_subst_in_return,
    script: "f() { return $(echo 42); }\nf\necho $?\n",
    eq: "42",
}

shell_compat! {
    name: return_without_value,
    script: "f() { echo hi; return; }\nX=$(f)\necho \"got: [$X]\"\n",
    eq: "got: [hi]",
}

// ---- Positional params in arithmetic --------------------------------------

shell_compat! {
    name: positional_params_arithmetic_add,
    script: "add() {\n    echo $(($1 + $2))\n}\nadd 3 4\n",
    eq: "7",
}

shell_compat! {
    name: positional_params_arithmetic_mul,
    script: "mul() {\n    echo $(($1 * $2))\n}\nmul 5 6\n",
    eq: "30",
}

// ---- export ---------------------------------------------------------------

shell_compat! {
    name: export_with_value,
    script: "export FOO=\"bar\"; echo $FOO",
    eq: "bar",
}

shell_compat! {
    name: export_multiple_vars,
    script: "export A=1; export B=2; echo \"$A $B\"",
    eq: "1 2",
}

// ---- Nested ${VAR:-default} -----------------------------------------------

shell_compat! {
    name: nested_var_default_all_unset,
    script: r#"echo "${A:-${B:-deep}}""#,
    eq: "deep",
}

shell_compat! {
    name: nested_var_default_outer_set,
    script: r#"A=outer; echo "${A:-${B:-deep}}""#,
    eq: "outer",
}

shell_compat! {
    name: nested_var_default_middle_set,
    script: r#"B=middle; echo "${A:-${B:-deep}}""#,
    eq: "middle",
}

shell_compat! {
    name: deeply_nested_var_defaults,
    script: r#"echo "${A:-${B:-${C:-deepest}}}""#,
    eq: "deepest",
}

// ---- Command substitution in [[ ]], case ----------------------------------

shell_compat! {
    name: cmd_subst_in_test_eq,
    script: r#"[[ $(echo hello) == "hello" ]] && echo "match" || echo "nope""#,
    eq: "match",
}

shell_compat! {
    name: cmd_subst_in_test_neq,
    script: r#"[[ $(echo hello) != "world" ]] && echo "diff" || echo "same""#,
    eq: "diff",
}

shell_compat! {
    name: cmd_subst_in_case,
    script: "case $(echo hello) in\n    hello) echo \"matched\" ;;\n    *) echo \"nope\" ;;\nesac\n",
    eq: "matched",
}

// ---- Compound test expressions: [[ A && B ]], [[ A || B ]], [[ ! A ]] ----
//
// Use `/` and `/tmp` (universal on POSIX-ish systems) instead of
// per-test tempdirs so the scripts are pure and deterministic on both sides.

shell_compat! {
    name: compound_and_both_true,
    script: r#"[[ -d / && -d /tmp ]] && echo "both""#,
    eq: "both",
}

shell_compat! {
    name: compound_and_one_false,
    script: r#"[[ -d / && -f /nonexistent_kaish_test ]] && echo "both" || echo "failed""#,
    eq: "failed",
}

shell_compat! {
    name: compound_or_first_true,
    script: r#"[[ -d / || -f /nonexistent_kaish_test ]] && echo "one""#,
    eq: "one",
}

shell_compat! {
    name: compound_or_second_true,
    script: r#"[[ -f /nonexistent_kaish_test || -d / ]] && echo "one""#,
    eq: "one",
}

shell_compat! {
    name: compound_or_both_false,
    script: r#"[[ -f /nonexistent_kaish_test || -f /also_nonexistent_kaish ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_not_true,
    script: r#"[[ ! -f /nonexistent_kaish_test ]] && echo "correct""#,
    eq: "correct",
}

shell_compat! {
    name: compound_not_false,
    script: r#"[[ ! -d / ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_double_not,
    script: r#"[[ ! ! -d / ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

shell_compat! {
    name: compound_string_tests,
    script: r#"VAR=""; [[ -z "$VAR" || -n "default" ]] && echo "ok""#,
    eq: "ok",
}

shell_compat! {
    name: compound_with_comparison,
    script: r#"X=5; [[ $X -gt 3 && $X -lt 10 ]] && echo "in range""#,
    eq: "in range",
}

shell_compat! {
    name: compound_in_if,
    script: "X=5\nif [[ $X -gt 0 && $X -lt 10 ]]; then\n    echo valid\nelse\n    echo invalid\nfi\n",
    eq: "valid",
}

shell_compat! {
    name: compound_short_circuit_or,
    script: r#"[[ -d / || $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

shell_compat! {
    name: compound_short_circuit_and,
    script: r#"[[ -f /nonexistent_kaish_test && $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_not_with_and,
    script: r#"[[ ! -f /nonexistent_kaish_test && -d / ]] && echo "both""#,
    eq: "both",
}

// Precedence: && binds tighter than ||
// [[ -f /nx || -d / && -d /tmp ]] = [[ -f /nx || (-d / && -d /tmp) ]]
// false || (true && true) = true
shell_compat! {
    name: compound_precedence,
    script: r#"[[ -f /nonexistent_kaish_test || -d / && -d /tmp ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

// ---- Tokenization edge cases ----------------------------------------------

shell_compat! {
    name: colon_in_unquoted_arg,
    script: "echo foo::bar",
    eq: "foo::bar",
}

shell_compat! {
    name: colon_port_in_arg,
    script: "echo host:8080",
    eq: "host:8080",
}

shell_compat! {
    name: colon_in_variable_assignment,
    script: "PATH=/usr/bin:/usr/local/bin\necho $PATH",
    eq: "/usr/bin:/usr/local/bin",
}

// ---- Pipelines (large-output deadlock regression) -------------------------

shell_compat! {
    name: pipeline_large_output_no_deadlock,
    script: "seq 1 20000 | wc -l",
    contains: "20000",
}

shell_compat! {
    name: head_n_in_pipeline,
    script: "printf 'a\\nb\\nc\\nd\\ne\\n' | head -n 3",
    eq: "a\nb\nc",
}

shell_compat! {
    name: pipeline_three_stage_large_output_no_deadlock,
    script: "seq 1 20000 | grep '1' | wc -l",
    eq: "13439",
}

// ---- Command-substitution scope/cwd isolation -----------------------------

shell_compat! {
    name: cmd_subst_variable_isolation,
    script: "X=outer\nset_inner() { X=inner; echo $X; }\nY=$(set_inner)\necho $X\n",
    eq: "outer",
}

// ---- for-loop semantics ---------------------------------------------------

shell_compat! {
    name: for_loop_cleanup_after_body_failure,
    script: "for i in 1 2 3; do\n    false\ndone\nfor j in a b c; do\n    echo $j\ndone\n",
    eq: "a\nb\nc",
}

// ---- set -e (errexit) -----------------------------------------------------

shell_compat! {
    name: errexit_after_and_chain,
    script: "set -e\ntrue && true\nfalse\necho \"should not reach\"\n",
    absent: "should not reach",
}

shell_compat! {
    name: errexit_after_or_chain,
    script: "set -e\nfalse || true\nfalse\necho \"should not reach\"\n",
    absent: "should not reach",
}

shell_compat! {
    name: errexit_suppressed_inside_and_chain_left,
    script: "set -e\nfalse && echo right\necho reached\n",
    contains: "reached",
    absent: "right",
}

// ---- Exit-code assertions -------------------------------------------------

shell_compat! {
    name: false_exits_one,
    script: "false",
    exit: 1,
}

shell_compat! {
    name: explicit_exit_code,
    script: "exit 7",
    exit: 7,
}

// ---- Recorded divergences -------------------------------------------------
//
// kaish does NOT split $(...) on whitespace; bash does. Capturing both
// expected outputs in one entry lets the test pass on each side AND fail
// loudly if either side ever changes its behavior unexpectedly.

shell_compat! {
    name: command_subst_no_implicit_split,
    script: "for x in $(echo \"a b c\"); do echo \"[$x]\"; done",
    kaish_eq: "[a b c]",
    bash_eq: "[a]\n[b]\n[c]",
}

// kaish scopes the for-loop variable to the loop frame; bash leaks the
// last bound value into the surrounding scope.

shell_compat! {
    name: for_loop_variable_scoping,
    script: "for i in 1 2 3; do\n    true\ndone\necho \"after: [$i]\"\n",
    kaish_eq: "after: []",
    bash_eq: "after: [3]",
}
