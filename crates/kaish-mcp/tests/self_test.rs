//! Self-hosting integration tests for kaish-mcp.
//!
//! These tests connect kaish-mcp to itself as an MCP server, exercising
//! the full client→server→kernel round-trip.

use std::sync::Arc;

use anyhow::{Context, Result};
use rmcp::model::RawContent;
use serde::{Deserialize, Serialize};
use serde_json::json;

use kaish_mcp::{McpClient, McpConfig, McpTransport};

/// Result from script execution (mirrors server::execute::ExecuteResult).
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ExecuteResult {
    code: i64,
    stdout: String,
    stderr: String,
    #[serde(default)]
    data: Option<serde_json::Value>,
    ok: bool,
}

/// Create a client connected to kaish-mcp.
async fn create_client() -> Result<Arc<McpClient>> {
    let client = McpClient::new(McpConfig {
        name: "kaish-self".into(),
        transport: McpTransport::Stdio {
            command: env!("CARGO_BIN_EXE_kaish-mcp").into(),
            args: vec![],
            env: vec![],
        },
    });

    client.connect().await.context("Failed to connect")?;
    Ok(Arc::new(client))
}

/// Execute a script via MCP and parse the result.
async fn execute_script(
    client: &McpClient,
    script: &str,
) -> Result<ExecuteResult> {
    execute_script_with_opts(client, script, None, None, None).await
}

/// Execute a script with options.
async fn execute_script_with_opts(
    client: &McpClient,
    script: &str,
    cwd: Option<&str>,
    env: Option<std::collections::HashMap<String, String>>,
    timeout_ms: Option<u64>,
) -> Result<ExecuteResult> {
    let mut args = serde_json::Map::new();
    args.insert("script".into(), json!(script));

    if let Some(cwd) = cwd {
        args.insert("cwd".into(), json!(cwd));
    }
    if let Some(env) = env {
        args.insert("env".into(), json!(env));
    }
    if let Some(timeout_ms) = timeout_ms {
        args.insert("timeout_ms".into(), json!(timeout_ms));
    }

    let result = client.call_tool("execute", Some(args)).await?;

    // Extract text content from the result
    let content = result
        .content
        .first()
        .context("No content in result")?;

    // The content is Annotated<RawContent>, access via .raw
    let text = match &content.raw {
        RawContent::Text(text_content) => &text_content.text,
        _ => anyhow::bail!("Expected text content"),
    };

    serde_json::from_str(text).context("Failed to parse ExecuteResult")
}

// =============================================================================
// Test Cases
// =============================================================================

#[tokio::test]
async fn test_tool_discovery() {
    let client = create_client().await.expect("client creation failed");

    let tools = client.list_tools().await.expect("list_tools failed");

    // Should have at least the execute tool
    let tool_names: Vec<_> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"execute"),
        "execute tool not found in: {:?}",
        tool_names
    );
}

#[tokio::test]
async fn test_basic_echo() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, "echo hello")
        .await
        .expect("execute failed");

    assert!(result.ok, "command should succeed");
    assert_eq!(result.code, 0);
    assert_eq!(result.stdout.trim(), "hello");
}

#[tokio::test]
async fn test_env_vars() {
    let client = create_client().await.expect("client creation failed");

    let mut env = std::collections::HashMap::new();
    env.insert("GREETING".into(), "world".into());

    let result = execute_script_with_opts(
        &client,
        r#"echo "hello ${GREETING}""#,
        None,
        Some(env),
        None,
    )
    .await
    .expect("execute failed");

    assert!(result.ok, "command should succeed");
    assert_eq!(result.stdout.trim(), "hello world");
}

#[tokio::test]
async fn test_json_output() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, r#"echo '{"count": 42}'"#)
        .await
        .expect("execute failed");

    assert!(result.ok, "command should succeed");
    assert!(result.data.is_some(), "should have parsed JSON data");

    let data = result.data.unwrap();
    assert_eq!(data.get("count"), Some(&json!(42)));
}

#[tokio::test]
async fn test_arithmetic() {
    let client = create_client().await.expect("client creation failed");

    // Basic arithmetic
    let result = execute_script(&client, "echo $((1 + 2))")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "3");

    // More complex expression
    let result = execute_script(&client, "echo $((2 * 3 + 4))")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "10");

    // Variable in arithmetic
    let result = execute_script(&client, "X=5; echo $((X * 2))")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "10");
}

#[tokio::test]
async fn test_control_flow_if() {
    let client = create_client().await.expect("client creation failed");

    // if/then/else
    let result = execute_script(
        &client,
        r#"
        X=10
        if [[ ${X} -gt 5 ]]; then
            echo "big"
        else
            echo "small"
        fi
    "#,
    )
    .await
    .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "big");
}

#[tokio::test]
async fn test_control_flow_for() {
    let client = create_client().await.expect("client creation failed");

    // for loop
    let result = execute_script(
        &client,
        r#"
        for I in 1 2 3; do
            echo "${I}"
        done
    "#,
    )
    .await
    .expect("execute failed");

    assert!(result.ok);
    // Filter empty lines since loop output may have extra newlines
    let lines: Vec<_> = result
        .stdout
        .trim()
        .lines()
        .filter(|l| !l.is_empty())
        .collect();
    assert_eq!(lines, vec!["1", "2", "3"]);
}

#[tokio::test]
async fn test_control_flow_while() {
    let client = create_client().await.expect("client creation failed");

    // while loop
    let result = execute_script(
        &client,
        r#"
        I=0
        while [[ ${I} -lt 3 ]]; do
            echo "${I}"
            I=$((I + 1))
        done
    "#,
    )
    .await
    .expect("execute failed");

    assert!(result.ok);
    // Filter empty lines since loop output may have extra newlines
    let lines: Vec<_> = result
        .stdout
        .trim()
        .lines()
        .filter(|l| !l.is_empty())
        .collect();
    assert_eq!(lines, vec!["0", "1", "2"]);
}

#[tokio::test]
async fn test_command_failure() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, "nonexistent_command_xyz")
        .await
        .expect("execute failed");

    assert!(!result.ok, "command should fail");
    assert_eq!(result.code, 127, "exit code should be 127 for not found");
}

#[tokio::test]
async fn test_timeout() {
    let client = create_client().await.expect("client creation failed");

    // A script that would take a long time without timeout
    let result = execute_script_with_opts(
        &client,
        r#"
            I=0
            while [[ ${I} -lt 1000000 ]]; do
                I=$((I + 1))
            done
        "#,
        None,
        None,
        Some(50), // 50ms timeout
    )
    .await
    .expect("execute failed");

    assert!(!result.ok, "should fail due to timeout");
    assert_eq!(result.code, 124, "exit code 124 indicates timeout");
    assert!(result.stderr.contains("timed out"));
}

#[tokio::test]
async fn test_multiple_executions() {
    let client = create_client().await.expect("client creation failed");

    // Run multiple commands on the same connection
    for i in 1..=5 {
        let result = execute_script(&client, &format!("echo {}", i))
            .await
            .expect("execute failed");

        assert!(result.ok);
        assert_eq!(result.stdout.trim(), i.to_string());
    }
}

#[tokio::test]
async fn test_exit_status_variable() {
    let client = create_client().await.expect("client creation failed");

    // Test $? captures previous exit status
    let result = execute_script(
        &client,
        r#"
        true
        echo $?
    "#,
    )
    .await
    .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "0");

    let result = execute_script(
        &client,
        r#"
        false
        echo $?
    "#,
    )
    .await
    .expect("execute failed");

    // Note: The final exit status is 0 because echo succeeded
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "1");
}

#[tokio::test]
async fn test_default_value_expansion() {
    let client = create_client().await.expect("client creation failed");

    // Variable with default when unset
    let result = execute_script(&client, r#"echo "${UNSET_VAR:-default}""#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "default");

    // Variable with default when set
    let result = execute_script(&client, r#"MYVAR=actual; echo "${MYVAR:-default}""#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "actual");
}

#[tokio::test]
async fn test_string_length() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, r#"VAR="hello"; echo ${#VAR}"#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "5");
}

#[tokio::test]
async fn test_printf_builtin() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, r#"printf "%s %d\n" hello 42"#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello 42");
}

#[tokio::test]
async fn test_true_false_builtins() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, "true; echo $?")
        .await
        .expect("execute failed");
    assert_eq!(result.stdout.trim(), "0");

    let result = execute_script(&client, "false; echo $?")
        .await
        .expect("execute failed");
    assert_eq!(result.stdout.trim(), "1");
}

#[tokio::test]
async fn test_variable_assignment_and_expansion() {
    let client = create_client().await.expect("client creation failed");

    // Simple assignment and expansion
    let result = execute_script(&client, "FOO=bar; echo ${FOO}")
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "bar");

    // Chained assignments
    let result = execute_script(&client, "A=1; B=2; C=$((A + B)); echo ${C}")
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "3");
}

#[tokio::test]
async fn test_quoted_strings() {
    let client = create_client().await.expect("client creation failed");

    // Single quotes (literal)
    let result = execute_script(&client, r#"VAR=world; echo 'hello ${VAR}'"#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello ${VAR}");

    // Double quotes (interpolated)
    let result = execute_script(&client, r#"VAR=world; echo "hello ${VAR}""#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello world");
}

#[tokio::test]
async fn test_resources_list() {
    let client = create_client().await.expect("client creation failed");

    // Access peer info to verify connection works
    let info = client.peer_info().await.expect("peer_info failed");
    assert!(info.contains("kaish"), "server info should mention kaish");
}

#[tokio::test]
async fn test_pipeline() {
    let client = create_client().await.expect("client creation failed");

    let result = execute_script(&client, r#"echo "hello world" | cat"#)
        .await
        .expect("execute failed");

    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello world");
}

#[tokio::test]
async fn test_command_chaining_and() {
    let client = create_client().await.expect("client creation failed");

    // && should short-circuit on failure
    let result = execute_script(&client, "true && echo success")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "success");

    let result = execute_script(&client, "false && echo success")
        .await
        .expect("execute failed");
    assert!(!result.ok);
    assert!(result.stdout.is_empty() || result.stdout.trim().is_empty());
}

#[tokio::test]
async fn test_command_chaining_or() {
    let client = create_client().await.expect("client creation failed");

    // || should short-circuit on success
    let result = execute_script(&client, "false || echo fallback")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "fallback");

    let result = execute_script(&client, "true || echo fallback")
        .await
        .expect("execute failed");
    assert!(result.ok);
    assert!(result.stdout.is_empty() || result.stdout.trim().is_empty());
}
