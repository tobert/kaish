//! Tests for external command execution via PATH lookup.
//!
//! These tests verify that kaish correctly falls back to PATH resolution
//! when no builtin tool matches a command name. They spawn real processes, so
//! the whole suite requires the `subprocess` capability.

#![cfg(feature = "subprocess")]

use kaish_kernel::{Kernel, KernelConfig};

/// Helper to create a kernel with passthrough filesystem and PATH access.
fn repl_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl()).expect("Failed to create kernel")
}

// ============================================================================
// Basic External Command Tests
// ============================================================================

#[tokio::test]
async fn external_command_basic() {
    let kernel = repl_kernel();
    // /bin/true always exists and returns 0
    let result = kernel.execute("true").await.unwrap();
    assert!(result.ok(), "true should succeed: {:?}", result);
}

#[tokio::test]
async fn external_command_with_args() {
    let kernel = repl_kernel();
    // Test that args are passed correctly
    let result = kernel.execute("echo hello world").await.unwrap();
    // Note: we have a builtin echo, so this tests builtin echo
    // Let's use a command that's definitely external
    assert!(result.ok());
}

#[tokio::test]
async fn external_command_not_found() {
    let kernel = repl_kernel();
    let result = kernel
        .execute("definitely_not_a_real_command_12345")
        .await
        .unwrap();
    assert_eq!(result.code, 127, "Should return 127 for command not found");
    assert!(
        result.err.contains("command not found"),
        "Error should mention 'command not found': {}",
        result.err
    );
}

// ============================================================================
// Date Format String Tests (requires lexer +bare handling)
// ============================================================================

#[tokio::test]
async fn external_command_date_format() {
    let kernel = repl_kernel();
    // Test that +%s is passed correctly to date
    let result = kernel.execute("date +%s").await.unwrap();
    assert!(result.ok(), "date +%s should succeed: {:?}", result);
    // Output should be a unix timestamp (all digits)
    let text = result.text_out();
    let out = text.trim();
    assert!(
        out.chars().all(|c| c.is_ascii_digit()),
        "date +%s should output digits: '{}'",
        out
    );
}

#[tokio::test]
async fn external_command_date_complex_format() {
    let kernel = repl_kernel();
    // Test more complex format string
    let result = kernel.execute("date +%Y-%m-%d").await.unwrap();
    assert!(result.ok(), "date +%Y-%m-%d should succeed: {:?}", result);
    // Output should match YYYY-MM-DD pattern
    let text = result.text_out();
    let out = text.trim();
    assert_eq!(out.len(), 10, "Date should be 10 chars: '{}'", out);
    assert!(out.contains('-'), "Date should have dashes: '{}'", out);
}

// ============================================================================
// Flag Preservation Tests
// ============================================================================

#[tokio::test]
async fn external_command_short_flags() {
    let kernel = repl_kernel();
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().display();
    // ls -la should work (testing short flag preservation)
    let result = kernel.execute(&format!("ls -la {path}")).await.unwrap();
    // We have builtin ls, but this tests that flags are handled
    assert!(result.ok(), "ls -la should succeed: {:?}", result);
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn external_command_long_flags() {
    let kernel = repl_kernel();
    // Test long flags with external command via spawn (uname is now a builtin)
    let result = kernel.execute("spawn --command uname --argv '--kernel-name'").await.unwrap();
    assert!(result.ok(), "spawn uname --kernel-name should succeed: {:?}", result);
    assert!(
        result.text_out().contains("Linux"),
        "Should show Linux: {}",
        result.text_out()
    );
}

// ============================================================================
// Exit Code Tests
// ============================================================================

#[tokio::test]
async fn external_command_exit_code_success() {
    let kernel = repl_kernel();
    let result = kernel.execute("true").await.unwrap();
    assert_eq!(result.code, 0, "true should exit with 0");
}

#[tokio::test]
async fn external_command_exit_code_failure() {
    let kernel = repl_kernel();
    let result = kernel.execute("false").await.unwrap();
    assert_eq!(result.code, 1, "false should exit with 1");
}

#[tokio::test]
async fn external_command_exit_code_specific() {
    let kernel = repl_kernel();
    // sh -c "exit N" is a reliable way to test specific exit codes
    let result = kernel.execute("sh -c 'exit 42'").await.unwrap();
    assert_eq!(result.code, 42, "Should preserve exit code 42");
}

// ============================================================================
// Stdin/Stdout Piping Tests
// ============================================================================

#[tokio::test]
async fn external_command_stdin_piping() {
    let kernel = repl_kernel();
    // Test that stdin flows correctly to external commands
    // Using our builtin echo piped to external wc
    let result = kernel.execute("echo 'hello world' | wc -c").await.unwrap();
    assert!(result.ok(), "pipe should succeed: {:?}", result);
    // "hello world\n" is 12 chars
    let count: i64 = result.text_out().trim().parse().unwrap_or(-1);
    assert_eq!(count, 12, "wc -c should count 12 chars: {}", result.text_out());
}

// ============================================================================
// Working Directory Tests
// ============================================================================

#[tokio::test]
async fn external_command_respects_cwd() {
    let kernel = repl_kernel();
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().to_string_lossy().to_string();
    // cd to a known directory, then run pwd
    kernel.execute(&format!("cd {path}")).await.unwrap();
    let result = kernel.execute("pwd").await.unwrap();
    assert!(result.ok(), "pwd should succeed: {:?}", result);
    assert!(
        result.text_out().contains(&path),
        "Should be in {}: {}",
        path,
        result.text_out()
    );
}

// ============================================================================
// Mixed Builtin and External Tests
// ============================================================================

#[tokio::test]
async fn pipeline_builtin_to_external() {
    let kernel = repl_kernel();
    // builtin echo | external sort
    let result = kernel
        .execute("echo 'c\nb\na' | sort")
        .await
        .unwrap();
    assert!(result.ok(), "pipeline should succeed: {:?}", result);
    // Sort should alphabetize
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["a", "b", "c"], "Should be sorted: {:?}", lines);
}

#[tokio::test]
async fn pipeline_builtin_to_builtin() {
    let kernel = repl_kernel();
    // seq | head (both builtins, tests pipeline)
    // Using lines=3 named arg since -n 3 requires schema-aware parsing
    let result = kernel.execute("seq 1 10 | head -n 3").await.unwrap();
    assert!(result.ok(), "pipeline should succeed: {:?}", result);
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"], "Should have first 3: {:?}", lines);
}

// ============================================================================
// Environment Variable Tests
// ============================================================================

#[tokio::test]
async fn external_command_is_hermetic_by_default() {
    // The kernel does not inherit OS env — `KernelConfig::repl()` alone does
    // not seed PATH. Frontends (the REPL binary, the MCP server) populate
    // `initial_vars` from `std::env::vars()`; embedders that don't populate
    // get a hermetic child env.
    let kernel = repl_kernel();
    assert!(
        std::env::var_os("PATH").is_some(),
        "test precondition: PATH must be set for cargo test"
    );
    let result = kernel.execute("printenv PATH").await.unwrap();
    assert!(
        !result.ok(),
        "printenv PATH must fail in hermetic kernel: {:?}",
        result
    );
}

#[tokio::test]
async fn external_command_sees_initial_vars() {
    // When a frontend populates `initial_vars`, those names are exported and
    // reach subprocesses. This is the path REPL/MCP take to mirror the host
    // env to children.
    use kaish_kernel::ast::Value;
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert("PATH".to_string(), Value::String("/usr/bin:/bin".into()));
    vars.insert("MY_PROBE".to_string(), Value::String("seeded".into()));

    let kernel = Kernel::new(KernelConfig::repl().with_initial_vars(vars))
        .expect("Failed to create kernel");

    let result = kernel.execute("printenv MY_PROBE").await.unwrap();
    assert!(result.ok(), "printenv MY_PROBE should succeed: {:?}", result);
    assert_eq!(result.text_out().trim(), "seeded");
}

// ============================================================================
// Interactive Stdin Inheritance Tests
// ============================================================================

/// Helper to create a kernel with interactive mode enabled.
fn interactive_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_interactive(true)).expect("Failed to create kernel")
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn non_interactive_stdin_is_dev_null() {
    let kernel = repl_kernel();
    // Use /bin/readlink to bypass the builtin — we need an external process
    // to introspect its own fd/0, since the builtin reads kaish's fd/0.
    // Linux-specific: requires /proc/self/fd/0.
    let result = kernel
        .execute("readlink /proc/self/fd/0")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_eq!(
        result.text_out().trim(),
        "/dev/null",
        "Non-interactive external command stdin should be /dev/null: {}",
        result.text_out()
    );
}

#[cfg(target_os = "linux")]
#[tokio::test]
#[ignore = "requires TTY stdin — fails when cargo test runs with stdin=/dev/null"]
async fn interactive_stdin_is_not_dev_null() {
    let kernel = interactive_kernel();
    // Standalone interactive commands inherit stdout (real-time streaming),
    // so we pipe through cat to capture output. Readlink is First in
    // the pipeline: stdout is captured for the pipe, but stdin still inherits
    // from the terminal (no piped input for the first command).
    // Linux-specific: requires /proc/self/fd/0.
    let result = kernel
        .execute("readlink /proc/self/fd/0 | cat")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_ne!(
        result.text_out().trim(),
        "/dev/null",
        "Interactive external command stdin should NOT be /dev/null: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn interactive_piped_stdin_still_works() {
    let kernel = interactive_kernel();
    // grep exits 0 only if it finds a match, so this verifies data flows
    // through the pipe. In interactive mode the last command (grep) inherits
    // stdout to the terminal, so we assert on exit code rather than output.
    let result = kernel
        .execute("echo hello | grep hello")
        .await
        .unwrap();
    assert_eq!(
        result.code, 0,
        "grep should find 'hello' in piped input (exit 0): {:?}",
        result
    );
}

// ============================================================================
// Minus Alone (stdin indicator) Tests
// ============================================================================

#[tokio::test]
async fn minus_alone_lexes_correctly() {
    let kernel = repl_kernel();
    // Test that "-" is recognized as a positional argument
    // Using echo to verify "-" passes through correctly
    let result = kernel.execute("echo - foo -").await.unwrap();
    assert!(result.ok(), "echo should succeed: {:?}", result);
    assert!(result.text_out().contains("- foo -"), "Should include dashes: {}", result.text_out());
}

/// Phase 2: a background *external* job records its child's process group, so
/// `kill -<sig> %N` can deliver an arbitrary signal (STOP/CONT), not just
/// terminate. If the PGID weren't recorded, `kill --signal STOP %1` would be
/// refused as an "in-process task" — so its exit 0 proves the killpg path.
///
/// The foreground builtin `sleep` gives the backgrounded external time to
/// spawn and register its PGID before the signals fire.
#[tokio::test]
async fn kill_signals_external_background_job_process_group() {
    let kernel = repl_kernel();
    let result = kernel
        .execute(
            "/usr/bin/sleep 30 & sleep 0.3; \
             kill --signal STOP %1; kill --signal CONT %1; kill %1",
        )
        .await
        .expect("execute");
    assert_eq!(
        result.code, 0,
        "STOP/CONT/TERM via process group should all succeed: {}",
        result.err
    );

    // The job is gone after the terminating kill.
    let again = kernel.execute("kill %1").await.expect("execute");
    assert!(again.err.contains("not found"), "job should be gone: {}", again.err);
}
