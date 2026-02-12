//! Edge case tests to probe for bugs in kaish-mcp.
//!
//! These tests intentionally try weird things to find issues.

use std::sync::Arc;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::json;

use kaish_mcp::{McpClient, McpConfig, McpTransport};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ExecuteResult {
    code: i64,
    stdout: String,
    stderr: String,
    #[serde(default)]
    data: Option<serde_json::Value>,
    ok: bool,
}

async fn create_client() -> Result<Arc<McpClient>> {
    let client = McpClient::new(McpConfig {
        name: "kaish-edge".into(),
        transport: McpTransport::Stdio {
            command: env!("CARGO_BIN_EXE_kaish-mcp").into(),
            args: vec![],
            env: vec![],
        },
    });
    client.connect().await.context("Failed to connect")?;
    Ok(Arc::new(client))
}

async fn execute(client: &McpClient, script: &str) -> Result<ExecuteResult> {
    let mut args = serde_json::Map::new();
    args.insert("script".into(), json!(script));
    let result = client.call_tool("execute", Some(args), None).await?;

    // structured_content is only present on error/stderr.
    // For clean success, reconstruct from content text blocks.
    if let Ok(typed) = result.clone().into_typed::<ExecuteResult>() {
        return Ok(typed);
    }

    let is_error = result.is_error.unwrap_or(false);
    let mut stdout = String::new();
    let mut stderr = String::new();
    for block in &result.content {
        if let Some(text) = block.raw.as_text() {
            if text.text.starts_with("[stderr] ") {
                stderr.push_str(text.text.trim_start_matches("[stderr] "));
            } else {
                stdout.push_str(&text.text);
            }
        }
    }

    Ok(ExecuteResult {
        code: if is_error { 1 } else { 0 },
        stdout,
        stderr,
        data: None,
        ok: !is_error,
    })
}

// =============================================================================
// Unicode & Special Characters
// =============================================================================

#[tokio::test]
async fn test_unicode_emoji() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "Hello 🦀 Rust 会sh""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "Hello 🦀 Rust 会sh");
}

#[tokio::test]
async fn test_unicode_cjk() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "日本語 中文 한국어""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "日本語 中文 한국어");
}

#[tokio::test]
async fn test_unicode_in_variable() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"NAME="会sh"; echo "Shell: ${NAME}""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "Shell: 会sh");
}

#[tokio::test]
async fn test_special_chars_in_string() {
    let client = create_client().await.unwrap();
    // Tabs, newlines in strings
    let result = execute(&client, r#"printf "a\tb\nc\n""#).await.unwrap();
    assert!(result.ok);
    assert!(result.stdout.contains('\t'));
    assert!(result.stdout.contains('\n'));
}

// =============================================================================
// Empty & Edge Values
// =============================================================================

#[tokio::test]
async fn test_empty_string_variable() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"EMPTY=""; echo ">${EMPTY}<""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "><");
}

#[tokio::test]
async fn test_empty_script() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.code, 0);
}

#[tokio::test]
async fn test_only_whitespace() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "   \n\t\n   ").await.unwrap();
    assert!(result.ok);
}

#[tokio::test]
async fn test_only_comments() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "# this is a comment\n# another one").await.unwrap();
    assert!(result.ok);
}

// =============================================================================
// Arithmetic Edge Cases
// =============================================================================

#[tokio::test]
async fn test_division_by_zero() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((10 / 0))").await.unwrap();
    // Should fail or return error - division by zero
    // Let's see what happens...
    println!("Division by zero result: {:?}", result);
}

#[tokio::test]
async fn test_modulo_by_zero() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((10 % 0))").await.unwrap();
    println!("Modulo by zero result: {:?}", result);
}

#[tokio::test]
async fn test_negative_numbers() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((-5 + 3))").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "-2");
}

#[tokio::test]
async fn test_large_numbers() {
    let client = create_client().await.unwrap();
    // i64 max is 9223372036854775807
    let result = execute(&client, "echo $((9223372036854775807))").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "9223372036854775807");
}

#[tokio::test]
async fn test_integer_overflow() {
    let client = create_client().await.unwrap();
    // What happens with overflow?
    let result = execute(&client, "echo $((9223372036854775807 + 1))").await.unwrap();
    println!("Overflow result: {:?}", result);
}

// =============================================================================
// Syntax Errors
// =============================================================================

#[tokio::test]
async fn test_syntax_error_unclosed_quote() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "unclosed"#).await.unwrap();
    assert!(!result.ok, "Should fail on unclosed quote");
    println!("Unclosed quote error: {}", result.stderr);
}

#[tokio::test]
async fn test_syntax_error_unclosed_brace() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo ${VAR").await.unwrap();
    assert!(!result.ok, "Should fail on unclosed brace");
    println!("Unclosed brace error: {}", result.stderr);
}

#[tokio::test]
async fn test_syntax_error_unclosed_paren() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((1 + 2)").await.unwrap();
    assert!(!result.ok, "Should fail on unclosed paren");
    println!("Unclosed paren error: {}", result.stderr);
}

#[tokio::test]
async fn test_syntax_error_bad_if() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "if true; then echo yes").await.unwrap();
    // Missing fi
    assert!(!result.ok, "Should fail on missing fi");
    println!("Missing fi error: {}", result.stderr);
}

// =============================================================================
// Quoting Edge Cases
// =============================================================================

#[tokio::test]
async fn test_nested_quotes() {
    let client = create_client().await.unwrap();
    // Single inside double
    let result = execute(&client, r#"echo "it's working""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "it's working");
}

#[tokio::test]
async fn test_escaped_quote() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "say \"hello\"""#).await.unwrap();
    println!("Escaped quote result: {:?}", result);
}

#[tokio::test]
async fn test_dollar_in_single_quotes() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo '$HOME'"#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "$HOME");
}

#[tokio::test]
async fn test_backslash_in_double_quotes() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "back\\slash""#).await.unwrap();
    println!("Backslash result: {:?}", result);
}

// =============================================================================
// Large Output
// =============================================================================

#[tokio::test]
async fn test_large_output() {
    let client = create_client().await.unwrap();
    // Generate 1000 lines of output
    let result = execute(&client, r#"
        I=0
        while [[ ${I} -lt 1000 ]]; do
            echo "Line ${I}: some content here to make it longer"
            I=$((I + 1))
        done
    "#).await.unwrap();

    assert!(result.ok);
    let line_count = result.stdout.lines().filter(|l| !l.is_empty()).count();
    assert_eq!(line_count, 1000, "Should have 1000 lines, got {}", line_count);
}

// =============================================================================
// State Isolation
// =============================================================================

#[tokio::test]
async fn test_state_isolation() {
    let client = create_client().await.unwrap();

    // Set a variable in first execution
    let result = execute(&client, "SECRET=hunter2; echo ${SECRET}").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hunter2");

    // Should NOT persist to second execution (fresh kernel each time)
    let result = execute(&client, r#"echo ">${SECRET}<""#).await.unwrap();
    assert!(result.ok);
    // Should be empty - variable shouldn't persist
    assert_eq!(result.stdout.trim(), "><", "Variable leaked between executions!");
}

// =============================================================================
// Concurrent Execution
// =============================================================================

#[tokio::test]
async fn test_concurrent_calls() {
    let client = create_client().await.unwrap();

    // Fire off 10 concurrent executions
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let client = Arc::clone(&client);
            tokio::spawn(async move {
                let script = format!("echo {}", i);
                let mut args = serde_json::Map::new();
                args.insert("script".into(), json!(script));
                client.call_tool("execute", Some(args), None).await
            })
        })
        .collect();

    let mut results = Vec::new();
    for handle in handles {
        results.push(handle.await.unwrap());
    }

    // All should succeed
    for (i, result) in results.iter().enumerate() {
        assert!(result.is_ok(), "Concurrent call {} failed: {:?}", i, result);
    }
}

// =============================================================================
// Special Variables
// =============================================================================

#[tokio::test]
async fn test_dollar_dollar_pid() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $$").await.unwrap();
    assert!(result.ok);
    // Should be a number
    let pid: i64 = result.stdout.trim().parse().expect("$$ should be a number");
    assert!(pid > 0, "$$ should be positive");
}

#[tokio::test]
async fn test_positional_params_empty() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $#").await.unwrap();
    assert!(result.ok);
    // No positional params in MCP context
    println!("$# result: {}", result.stdout.trim());
}

// =============================================================================
// Pipeline Edge Cases
// =============================================================================

#[tokio::test]
async fn test_pipeline_with_failure() {
    let client = create_client().await.unwrap();
    // First command fails
    let result = execute(&client, "false | echo piped").await.unwrap();
    println!("Pipeline with failure: {:?}", result);
}

#[tokio::test]
async fn test_long_pipeline() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo hello | cat | cat | cat | cat").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello");
}

// =============================================================================
// Weird But Valid
// =============================================================================

#[tokio::test]
async fn test_semicolon_only() {
    let client = create_client().await.unwrap();
    let result = execute(&client, ";;;").await.unwrap();
    println!("Semicolons only: {:?}", result);
}

#[tokio::test]
async fn test_many_semicolons() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo a;; echo b;;; echo c").await.unwrap();
    println!("Many semicolons: {:?}", result);
}

#[tokio::test]
async fn test_deeply_nested_arithmetic() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((((((1 + 2) * 3) - 4) / 2) + 1))").await.unwrap();
    assert!(result.ok);
    // ((((1+2)*3)-4)/2)+1 = (((3*3)-4)/2)+1 = ((9-4)/2)+1 = (5/2)+1 = 2+1 = 3
    assert_eq!(result.stdout.trim(), "3");
}

#[tokio::test]
async fn test_variable_in_variable_name() {
    let client = create_client().await.unwrap();
    // This is indirect expansion - ${!var} - probably not supported
    let result = execute(&client, r#"NAME=FOO; FOO=bar; echo "${!NAME}""#).await.unwrap();
    println!("Indirect expansion: {:?}", result);
}
