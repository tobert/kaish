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
// PID forms: $$ and ${$} — values can't be compared cross-process
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
    // Should be a positive integer (the PID)
    let pid: u32 = result
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
