//! Tests for external command execution via PATH lookup.
//!
//! These tests verify that kaish correctly falls back to PATH resolution
//! when no builtin tool matches a command name.

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
    let out = result.out.trim();
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
    let out = result.out.trim();
    assert_eq!(out.len(), 10, "Date should be 10 chars: '{}'", out);
    assert!(out.contains('-'), "Date should have dashes: '{}'", out);
}

// ============================================================================
// Flag Preservation Tests
// ============================================================================

#[tokio::test]
async fn external_command_short_flags() {
    let kernel = repl_kernel();
    // ls -la should work (testing short flag preservation)
    let result = kernel.execute("ls -la /tmp").await.unwrap();
    // We have builtin ls, but this tests that flags are handled
    assert!(result.ok(), "ls -la should succeed: {:?}", result);
}

#[tokio::test]
async fn external_command_long_flags() {
    let kernel = repl_kernel();
    // Test long flags with external command via spawn (uname is now a builtin)
    let result = kernel.execute("spawn command=uname argv='--kernel-name'").await.unwrap();
    assert!(result.ok(), "spawn uname --kernel-name should succeed: {:?}", result);
    assert!(
        result.out.contains("Linux"),
        "Should show Linux: {}",
        result.out
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
    let count: i64 = result.out.trim().parse().unwrap_or(-1);
    assert_eq!(count, 12, "wc -c should count 12 chars: {}", result.out);
}

// ============================================================================
// Working Directory Tests
// ============================================================================

#[tokio::test]
async fn external_command_respects_cwd() {
    let kernel = repl_kernel();
    // cd to a known directory, then run pwd
    kernel.execute("cd /tmp").await.unwrap();
    let result = kernel.execute("pwd").await.unwrap();
    assert!(result.ok(), "pwd should succeed: {:?}", result);
    assert!(
        result.out.contains("/tmp"),
        "Should be in /tmp: {}",
        result.out
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
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert_eq!(lines, vec!["a", "b", "c"], "Should be sorted: {:?}", lines);
}

#[tokio::test]
async fn pipeline_builtin_to_builtin() {
    let kernel = repl_kernel();
    // seq | head (both builtins, tests pipeline)
    // Using lines=3 named arg since -n 3 requires schema-aware parsing
    let result = kernel.execute("seq 1 10 | head lines=3").await.unwrap();
    assert!(result.ok(), "pipeline should succeed: {:?}", result);
    let lines: Vec<&str> = result.out.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"], "Should have first 3: {:?}", lines);
}

// ============================================================================
// Environment Variable Tests
// ============================================================================

#[tokio::test]
async fn external_command_inherits_env() {
    let kernel = repl_kernel();
    // Test that external commands can see process environment
    // We use printenv which is typically external
    let result = kernel.execute("printenv HOME").await.unwrap();
    assert!(result.ok(), "printenv should succeed: {:?}", result);
    assert!(
        !result.out.trim().is_empty(),
        "Should see HOME: {}",
        result.out
    );
}

// ============================================================================
// Interactive Stdin Inheritance Tests
// ============================================================================

/// Helper to create a kernel with interactive mode enabled.
fn interactive_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_interactive(true)).expect("Failed to create kernel")
}

#[tokio::test]
async fn non_interactive_stdin_is_dev_null() {
    let kernel = repl_kernel();
    // Use /bin/readlink to bypass the builtin — we need an external process
    // to introspect its own fd/0, since the builtin reads kaish's fd/0.
    let result = kernel
        .execute("/bin/readlink /proc/self/fd/0")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_eq!(
        result.out.trim(),
        "/dev/null",
        "Non-interactive external command stdin should be /dev/null: {}",
        result.out
    );
}

#[tokio::test]
#[ignore = "requires TTY stdin — fails when cargo test runs with stdin=/dev/null"]
async fn interactive_stdin_is_not_dev_null() {
    let kernel = interactive_kernel();
    // Standalone interactive commands inherit stdout (real-time streaming),
    // so we pipe through /bin/cat to capture output. Readlink is First in
    // the pipeline: stdout is captured for the pipe, but stdin still inherits
    // from the terminal (no piped input for the first command).
    let result = kernel
        .execute("/bin/readlink /proc/self/fd/0 | /bin/cat")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_ne!(
        result.out.trim(),
        "/dev/null",
        "Interactive external command stdin should NOT be /dev/null: {}",
        result.out
    );
}

#[tokio::test]
async fn interactive_piped_stdin_still_works() {
    let kernel = interactive_kernel();
    // grep exits 0 only if it finds a match, so this verifies data flows
    // through the pipe. In interactive mode the last command (grep) inherits
    // stdout to the terminal, so we assert on exit code rather than output.
    let result = kernel
        .execute("echo hello | /bin/grep hello")
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
    assert!(result.out.contains("- foo -"), "Should include dashes: {}", result.out);
}
