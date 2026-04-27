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

mod common;

// ---- Cases where kaish and bash agree -------------------------------------

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
