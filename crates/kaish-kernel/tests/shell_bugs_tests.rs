//! Kaish-specific shell behavior tests that don't translate to bash.
//!
//! Bash-portable scenarios live in `shell_compat_tests.rs` (which runs each
//! script through both kaish AND `bash -c` when `KAISH_BASH_COMPAT=1` is set,
//! catching divergences). This file is the home for behaviors that are
//! kaish-only by design or otherwise can't be directly compared:
//!
//! - `local x = inner` — kaish syntax with surrounding spaces; bash uses
//!   `local x=inner` and rejects the spaced form.
//! - `$$` / `${$}` — process IDs differ across processes, so kaish vs.
//!   `bash -c` can't agree on a value.
//! - Structured iteration: `split`, `seq` returning arrays.
//! - Stderr redirects (`>&2`, `1>&2`) — the compat harness only inspects
//!   stdout today.
//! - VFS-only paths (`/v/...`) and the `find` builtin's filtering through
//!   the parser.
//! - Short flags-with-value tests that depend on a real tempfile.
//! - Command-substitution cwd isolation, which depends on the host process's
//!   starting cwd (hard to make deterministic across two runners).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::Kernel;

// ============================================================================
// Within-kaish equivalence: $? braced vs unbraced
// ============================================================================

#[tokio::test]
async fn test_braced_vs_unbraced_exit_code_equivalence() {
    let kernel = Kernel::transient().unwrap();
    // Both forms should give the same result
    let result1 = kernel.execute("false; echo $?").await.unwrap();
    let result2 = kernel.execute("false; echo ${?}").await.unwrap();
    assert_eq!(
        result1.text_out().trim(),
        result2.text_out().trim(),
        "Braced and unbraced $? should be equivalent"
    );
}

// ============================================================================
// $$ / ${$} — kaish-internal session identifier (monotonic u64 counter)
//
// kaish exposes $$ as a per-Kernel counter, *not* the OS PID. Compat with
// bash via `bash -c` would always disagree (different processes), so these
// live here rather than in the compat suite. See
// memory/lang_dollar_dollar_identifier.md for the rationale.
// ============================================================================

#[tokio::test]
async fn test_pid_in_arithmetic() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo $(( $$ % 100000 ))").await.unwrap();
    let val: i64 = result.text_out().trim().parse().expect("Should be a number");
    assert!(val >= 0, "PID mod should be non-negative");
}

#[tokio::test]
async fn test_braced_current_pid() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo ${$}").await.unwrap();
    let pid: u64 = result
        .text_out()
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
        result1.text_out().trim(),
        result2.text_out().trim(),
        "Braced and unbraced $$ should be equivalent"
    );
}

#[tokio::test]
async fn test_two_kernels_have_distinct_pids() {
    // Each Kernel construction takes the next counter value, so two
    // kernels created in the same test run must report different $$.
    let k1 = Kernel::transient().unwrap();
    let k2 = Kernel::transient().unwrap();
    let p1 = k1.execute("echo $$").await.unwrap().text_out().trim().to_string();
    let p2 = k2.execute("echo $$").await.unwrap().text_out().trim().to_string();
    assert_ne!(p1, p2, "two kernels should have different $$, got {p1} and {p2}");
}

#[tokio::test]
async fn test_fork_inherits_parent_pid() {
    // A forked subkernel must share its parent's $$ — matches bash's
    // "subshell keeps parent's $$" semantics.
    let parent = Kernel::transient().unwrap();
    let parent_pid = parent
        .execute("echo $$")
        .await
        .unwrap()
        .text_out()
        .trim()
        .to_string();
    let fork = parent.fork().await;
    let fork_pid = fork
        .execute("echo $$")
        .await
        .unwrap()
        .text_out()
        .trim()
        .to_string();
    assert_eq!(parent_pid, fork_pid, "fork should inherit parent $$");
}

#[tokio::test]
async fn test_kaish_clear_preserves_pid() {
    // kaish-clear resets variables/cwd but the kernel hasn't restarted —
    // $$ should be the same identifier before and after.
    let kernel = Kernel::transient().unwrap();
    let before = kernel.execute("echo $$").await.unwrap().text_out().trim().to_string();
    kernel.execute("kaish-clear").await.unwrap();
    let after = kernel.execute("echo $$").await.unwrap().text_out().trim().to_string();
    assert_eq!(before, after, "kaish-clear should preserve $$");
}

// ============================================================================
// `local x = value` — kaish syntax with surrounding spaces (bash uses `x=v`)
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
        result.text_out().contains("in func: inner"),
        "Local var should be 'inner' inside function: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("after func: outer"),
        "Outer var should be 'outer' after function: {}",
        result.text_out()
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
        result.text_out().trim(),
        "0",
        "Outer 'count' should still be 0: {}",
        result.text_out()
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
        result.text_out().trim(),
        "99",
        "Without local, 'count' should be modified: {}",
        result.text_out()
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
        result.text_out().contains("local: from_cmd"),
        "Local with cmd subst: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("outer: original"),
        "Outer unchanged: {}",
        result.text_out()
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
        result.text_out().trim(),
        "15",
        "$(($base + $1)) with base=10 and $1=5 should be 15: {}",
        result.text_out()
    );
}

// ============================================================================
// Structured iteration: kaish split / seq return arrays (no implicit splitting)
// ============================================================================

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
        result.text_out().contains("item: a"),
        "Should have item a: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("item: b"),
        "Should have item b: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("item: c"),
        "Should have item c: {}",
        result.text_out()
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
        result.text_out().contains("number 1"),
        "Should have number 1: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("number 2"),
        "Should have number 2: {}",
        result.text_out()
    );
    assert!(
        result.text_out().contains("number 3"),
        "Should have number 3: {}",
        result.text_out()
    );
}

// ============================================================================
// Stderr redirects: 1>&2 and >&2 — compat harness only inspects stdout today
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
        result.text_out(),
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
        result.text_out(),
        result.err
    );
}

// ============================================================================
// VFS file tests — [[ -d / -f / -e ]] against `/v/...` paths
// ============================================================================

#[tokio::test]
async fn test_file_test_sees_vfs_dirs() {
    let kernel = Kernel::transient().unwrap();
    kernel.execute("mkdir -p /v/testdir").await.unwrap();
    let result = kernel.execute(r#"[[ -d /v/testdir ]] && echo "found" || echo "missing""#).await.unwrap();
    assert_eq!(result.text_out().trim(), "found", "[[ -d ]] should see VFS dirs: {}", result.text_out());
}

#[tokio::test]
async fn test_file_test_sees_vfs_files() {
    let kernel = Kernel::transient().unwrap();
    kernel.execute("write /v/testfile 'hello'").await.unwrap();
    let result = kernel.execute(r#"[[ -f /v/testfile ]] && echo "found" || echo "missing""#).await.unwrap();
    assert_eq!(result.text_out().trim(), "found", "[[ -f ]] should see VFS files: {}", result.text_out());
}

#[tokio::test]
async fn test_file_test_exists_vfs() {
    let kernel = Kernel::transient().unwrap();
    kernel.execute("write /v/somefile 'data'").await.unwrap();
    let result = kernel.execute(r#"[[ -e /v/somefile ]] && echo "found" || echo "missing""#).await.unwrap();
    assert_eq!(result.text_out().trim(), "found", "[[ -e ]] should see VFS entries: {}", result.text_out());
}

// ============================================================================
// Short flags with values (head -n N, tail -n N) — depend on a real tempfile
// ============================================================================

#[tokio::test]
async fn test_short_flag_with_value_head() {
    let kernel = Kernel::transient().unwrap();
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let path = tmp.path().display();
    kernel.execute(&format!(r#"printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" > {path}"#)).await.unwrap();

    // head -n 3 should return first 3 lines
    let result = kernel.execute(&format!("head -n 3 {path}")).await.unwrap();
    assert!(result.ok(), "head -n 3 should succeed: err={}", result.err);
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"], "head -n 3 should return first 3 lines, got: {:?}", lines);
}

#[tokio::test]
async fn test_short_flag_with_value_tail() {
    let kernel = Kernel::transient().unwrap();
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let path = tmp.path().display();
    kernel.execute(&format!(r#"printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" > {path}"#)).await.unwrap();

    // tail -n 3 should return last 3 lines
    let result = kernel.execute(&format!("tail -n 3 {path}")).await.unwrap();
    assert!(result.ok(), "tail -n 3 should succeed: err={}", result.err);
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["8", "9", "10"], "tail -n 3 should return last 3 lines, got: {:?}", lines);
}

// ============================================================================
// Command-substitution cwd isolation — depends on host process cwd
// ============================================================================

// Without `localfs`, `Kernel::transient()` falls back to `KernelConfig::isolated()`
// (NoLocal), whose cwd is always literally "/" — the "pwd should not leak back to
// the subshell's cd /" assertion below is structurally unsatisfiable there since
// the kernel's own starting cwd already is "/". Needs a real (non-"/") starting
// cwd, which only `localfs`'s sandbox root provides.
#[cfg(feature = "localfs")]
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
    let text = result.text_out();
    let pwd_output = text.trim();
    assert_ne!(
        pwd_output, "/",
        "CWD should not leak from command substitution, got: {}",
        pwd_output
    );
}

// Same NoLocal-cwd-is-always-"/" reasoning as `test_cmd_subst_cwd_isolation` above.
#[cfg(feature = "localfs")]
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
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
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
        result.text_out().contains("captured: /"),
        "Should capture '/' from subshell: {}",
        result.text_out()
    );
}

// ============================================================================
// find through the parser — VFS-only paths
// ============================================================================

#[tokio::test]
async fn test_find_name_filter_through_parser() {
    let kernel = Kernel::transient().unwrap();

    // Set up test files in /v/ (ephemeral memory VFS)
    kernel
        .execute("mkdir -p /v/proj/src; mkdir -p /v/proj/docs")
        .await
        .unwrap();
    kernel
        .execute(
            "write /v/proj/src/main.rs 'fn main() {}'; \
             write /v/proj/src/lib.rs 'pub mod lib;'; \
             write /v/proj/docs/README.md '# Docs'; \
             write /v/proj/Cargo.toml '[package]'",
        )
        .await
        .unwrap();

    // find with -name should only return .rs files
    let result = kernel
        .execute("find /v/proj -name '*.rs'")
        .await
        .unwrap();
    assert!(result.ok(), "find should succeed: {}", result.err);

    let out = result.text_out();
    assert!(out.contains("main.rs"), "should find main.rs: {out}");
    assert!(out.contains("lib.rs"), "should find lib.rs: {out}");
    assert!(
        !out.contains("README.md"),
        "-name '*.rs' should exclude README.md: {out}"
    );
    assert!(
        !out.contains("Cargo.toml"),
        "-name '*.rs' should exclude Cargo.toml: {out}"
    );
}

#[tokio::test]
async fn test_find_type_filter_through_parser() {
    let kernel = Kernel::transient().unwrap();

    kernel
        .execute("mkdir -p /v/proj/src; write /v/proj/src/main.rs 'fn main() {}'")
        .await
        .unwrap();

    // -type f: files only, no directories
    let files_result = kernel
        .execute("find /v/proj -type f")
        .await
        .unwrap();
    assert!(files_result.ok(), "find -type f should succeed: {}", files_result.err);
    let files_out = files_result.text_out();
    assert!(
        files_out.contains("main.rs"),
        "-type f should include files: {files_out}"
    );
    assert!(
        !files_out.contains("/src\n"),
        "-type f should exclude directories: {files_out}"
    );

    // -type d: directories only, no files
    let dirs_result = kernel
        .execute("find /v/proj -type d")
        .await
        .unwrap();
    assert!(dirs_result.ok(), "find -type d should succeed: {}", dirs_result.err);
    let dirs_out = dirs_result.text_out();
    assert!(
        dirs_out.contains("src"),
        "-type d should include directories: {dirs_out}"
    );
    assert!(
        !dirs_out.contains("main.rs"),
        "-type d should exclude files: {dirs_out}"
    );
}

// ============================================================================
// Bareword argv tokens: digit-leading and dot-prefixed strings
//
// These forms are common in real-world argv: SHA prefixes (`019dda1c`),
// hidden files (`.gitignore`), and context refs (`.parent`, `.parent.parent`).
// The lexer must produce a single bareword token for each so they aren't
// rejected (digit-leading) or misparsed as the POSIX `.` source alias
// (dot-prefixed).
// ============================================================================

#[tokio::test]
async fn test_argv_digit_leading_hex_passes_through() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"echo 019dda1c "msg""#)
        .await
        .expect("parse + exec should succeed for digit-leading bareword");
    assert!(result.ok(), "echo failed: err={}", result.err);
    assert_eq!(result.text_out().trim(), "019dda1c msg");
}

#[tokio::test]
async fn test_argv_digit_leading_uuid_passes_through() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("echo 019dda1c-5b3f-7000-abcd-0123456789ab")
        .await
        .expect("UUID-like bareword should lex");
    assert!(result.ok(), "echo failed: err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "019dda1c-5b3f-7000-abcd-0123456789ab"
    );
}

#[tokio::test]
async fn test_argv_dot_prefixed_simple() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"echo .parent "msg""#)
        .await
        .expect("dot-prefixed bareword should lex");
    assert!(result.ok(), "echo failed: err={}", result.err);
    assert_eq!(result.text_out().trim(), ".parent msg");
}

#[tokio::test]
async fn test_argv_dot_prefixed_chained() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("echo .parent.parent")
        .await
        .expect("chained .parent.parent should lex as one token");
    assert!(result.ok(), "echo failed: err={}", result.err);
    assert_eq!(result.text_out().trim(), ".parent.parent");
}

#[tokio::test]
async fn test_argv_dot_prefixed_hidden_file() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("echo .gitignore")
        .await
        .expect("hidden filename should not be parsed as source command");
    assert!(result.ok(), "echo failed: err={}", result.err);
    assert_eq!(result.text_out().trim(), ".gitignore");
}

// `. file` (with whitespace) must still be the source alias — POSIX behavior.
#[tokio::test]
async fn test_source_alias_with_space_still_works() {
    let kernel = Kernel::transient().unwrap();
    // Trying to source a non-existent file should hit the source builtin's
    // missing-file error, not silently succeed.
    let result = kernel
        .execute(". /nonexistent-kaish-source-test")
        .await
        .expect("`. file` should still be parsed as source command");
    assert!(
        !result.ok(),
        "source of nonexistent file should fail (proves source ran)"
    );
    assert!(
        result.err.contains("source") || result.err.contains("not found")
            || result.err.contains("No such file"),
        "expected source error, got: stdout={}, stderr={}",
        result.text_out(),
        result.err,
    );
}

// ============================================================================
// Top-level `return` in a skip-validation embedder context must stop the
// script and keep prior statements' accumulated output, like `exit` — not
// silently discard it and keep running. Ordinary kernels reject a top-level
// `return` at validation time (`ReturnOutsideFunction`), but an embedder that
// opts into `with_skip_validation(true)` (a supported, commonly-used config —
// see the many test files that build kernels this way) can still reach this
// path, so `execute_streaming_inner`'s handling of it must be correct too.
// ============================================================================

#[tokio::test]
async fn test_top_level_return_keeps_prior_output_and_stops_execution() {
    use kaish_kernel::KernelConfig;
    let kernel = Kernel::new(KernelConfig::transient().with_skip_validation(true)).unwrap();
    let result = kernel
        .execute("echo setup-ok; return 0; echo after")
        .await
        .expect("skip-validation kernel should run a top-level return");
    assert_eq!(result.code, 0);
    assert!(
        result.text_out().contains("setup-ok"),
        "return must not discard prior statements' output: {:?}",
        result.text_out()
    );
    assert!(
        !result.text_out().contains("after"),
        "return must stop execution of subsequent statements: {:?}",
        result.text_out()
    );
}

// ============================================================================
// `!` precedence in `[[ ]]` — `!` binds tighter than `&&`/`||`
//
// Regression for the 2026-06-09 finding: the parser bound `!` to the entire
// rest of the expression (`!(A || B)`) instead of just the next term
// (`(!A) || B`), contradicting both LANGUAGE.md and the grammar comment in
// `parser.rs`. String tests keep these filesystem-independent.
// ============================================================================

/// Run `[[ <expr> ]]` and return true iff it evaluates true (if-branch taken).
async fn test_truthy(expr: &str) -> bool {
    let kernel = Kernel::transient().unwrap();
    let script = format!("if [[ {expr} ]]; then echo Y; else echo N; fi");
    let out = kernel.execute(&script).await.unwrap();
    match out.text_out().trim() {
        "Y" => true,
        "N" => false,
        other => panic!("unexpected output {other:?} for [[ {expr} ]]"),
    }
}

#[tokio::test]
async fn test_bang_binds_tighter_than_or() {
    // (!false) || true  == true.  Buggy `!(false || true)` == false.
    assert!(
        test_truthy(r#"! "a" == "b" || "a" == "a""#).await,
        "`! A || B` must parse as `(!A) || B`"
    );
    // (!true) || true == true.  Buggy `!(true || true)` == false.
    assert!(
        test_truthy(r#"! "a" == "a" || "b" == "b""#).await,
        "`! A || B` with A true must still be `(!A) || B`"
    );
}

#[tokio::test]
async fn test_bang_binds_tighter_than_and() {
    // (!true) && true == false.
    assert!(
        !test_truthy(r#"! "a" == "a" && "b" == "b""#).await,
        "`! A && B` must parse as `(!A) && B`"
    );
    // (!false) && true == true.
    assert!(
        test_truthy(r#"! "a" == "b" && "b" == "b""#).await,
        "`! A && B` with A false is `(!A) && B` == true"
    );
}

#[tokio::test]
async fn test_bang_single_term_unaffected() {
    assert!(test_truthy(r#"! "a" == "b""#).await, "!(a==b) is true");
    assert!(!test_truthy(r#"! "a" == "a""#).await, "!(a==a) is false");
    // Double negation still chains at the unary level.
    assert!(test_truthy(r#"! ! "a" == "a""#).await, "!!(a==a) is true");
}

// ============================================================================
// `--` end-of-flags protects dash-words with internal hyphens.
//
// `-not-a-flag` is one shell word; it used to fragment into three flag tokens
// (`-not` `-a` `-flag`) at the lexer, so `echo -- -not-a-flag` printed
// `-not -a -flag`. kaish consumes the `--` terminator (a deliberate divergence
// from bash's echo, which keeps it), so this is kaish-specific.
// ============================================================================

#[tokio::test]
async fn test_double_dash_protects_internal_hyphen_word() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo -- -not-a-flag").await.unwrap();
    assert_eq!(result.text_out().trim(), "-not-a-flag");
}

#[tokio::test]
async fn test_double_dash_protects_multiple_dash_words() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo -- -a-b -c-d").await.unwrap();
    assert_eq!(result.text_out().trim(), "-a-b -c-d");
}

// ============================================================================
// `--` end-of-flags does not choke on an assignment-shaped word.
//
// After `--`, nothing is special — including `=` — so `a=b` is one literal
// positional word, matching standard shell behavior. The post-`--` argument
// grammar previously had no rule to absorb a bareword `key=value` token
// (`Arg::WordAssign`'s `Ident`+`Eq`+expr production only ran pre-`--`), so the
// parser hard-failed on the unhandled `=` instead of producing a positional.
// ============================================================================

#[tokio::test]
async fn test_double_dash_allows_bareword_assignment() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo -- a=b").await.unwrap();
    assert_eq!(result.text_out().trim(), "a=b");
}

#[tokio::test]
async fn test_double_dash_allows_multiple_bareword_assignments() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo -- a=b c=d").await.unwrap();
    assert_eq!(result.text_out().trim(), "a=b c=d");
}

