//! Stress tests to really try to break things.

use std::sync::Arc;

use anyhow::{Context, Result};
use rmcp::model::RawContent;
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
        name: "kaish-stress".into(),
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
    let result = client.call_tool("execute", Some(args)).await?;
    let content = result.content.first().context("No content")?;
    let text = match &content.raw {
        RawContent::Text(t) => &t.text,
        _ => anyhow::bail!("Expected text"),
    };
    serde_json::from_str(text).context("Failed to parse")
}

// =============================================================================
// Pathological Input
// =============================================================================

#[tokio::test]
async fn test_very_long_variable_name() {
    let client = create_client().await.unwrap();
    let long_name = "A".repeat(10000);
    let script = format!("{}=hello; echo ${{{}}}", long_name, long_name);
    let result = execute(&client, &script).await.unwrap();
    println!("Long var name: ok={}, code={}", result.ok, result.code);
}

#[tokio::test]
#[ignore = "MCP transport closes on very large payloads (100KB+)"]
async fn test_very_long_string() {
    let client = create_client().await.unwrap();
    let long_string = "x".repeat(100_000);
    let script = format!(r#"echo "{}""#, long_string);
    let result = execute(&client, &script).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim().len(), 100_000);
}

#[tokio::test]
#[ignore = "kernel: stack overflow in execute_stmt_flow with 1000 statements"]
async fn test_many_variables() {
    let client = create_client().await.unwrap();
    // Create 1000 variables
    let mut script = String::new();
    for i in 0..1000 {
        script.push_str(&format!("VAR{}={}; ", i, i));
    }
    script.push_str("echo done");
    let result = execute(&client, &script).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "done");
}

#[tokio::test]
#[ignore = "kernel: nested ${VAR:-default} expansion not recursive"]
async fn test_deeply_nested_braces() {
    let client = create_client().await.unwrap();
    // ${VAR:-${VAR2:-${VAR3:-default}}}
    let script = r#"echo "${A:-${B:-${C:-${D:-${E:-deep}}}}}""#;
    let result = execute(&client, &script).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "deep");
}

#[tokio::test]
async fn test_many_pipes() {
    let client = create_client().await.unwrap();
    // 20 pipes
    let mut script = "echo hello".to_string();
    for _ in 0..20 {
        script.push_str(" | cat");
    }
    let result = execute(&client, &script).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hello");
}

#[tokio::test]
async fn test_many_semicolons_stress() {
    let client = create_client().await.unwrap();
    // 1000 echo commands
    let script = (0..1000).map(|i| format!("echo {}", i)).collect::<Vec<_>>().join("; ");
    let result = execute(&client, &script).await.unwrap();
    assert!(result.ok);
    let lines: Vec<_> = result.stdout.trim().lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(lines.len(), 1000);
}

// =============================================================================
// Weird Parsing Edge Cases
// =============================================================================

#[tokio::test]
async fn test_equals_in_value() {
    let client = create_client().await.unwrap();
    // The value itself contains =
    let result = execute(&client, r#"URL="http://example.com?foo=bar"; echo ${URL}"#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "http://example.com?foo=bar");
}

#[tokio::test]
async fn test_dollar_at_end() {
    let client = create_client().await.unwrap();
    // Bare $ at end of string
    let result = execute(&client, r#"echo "cost: $""#).await.unwrap();
    println!("Dollar at end: {:?}", result);
}

#[tokio::test]
async fn test_empty_expansion() {
    let client = create_client().await.unwrap();
    // ${}
    let result = execute(&client, r#"echo "${}" "#).await.unwrap();
    println!("Empty expansion: {:?}", result);
}

#[tokio::test]
async fn test_newline_in_string() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo \"line1\nline2\"").await.unwrap();
    assert!(result.ok);
    // Should have actual newline in output
    assert!(result.stdout.contains('\n'));
}

#[tokio::test]
async fn test_tab_in_value() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "VAR=\"a\tb\"; echo ${VAR}").await.unwrap();
    println!("Tab in value: {:?}", result);
}

#[tokio::test]
async fn test_hash_in_string() {
    let client = create_client().await.unwrap();
    // # normally starts a comment, but not in quotes
    let result = execute(&client, r#"echo "hashtag #yolo""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "hashtag #yolo");
}

#[tokio::test]
async fn test_semicolon_in_string() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "a;b;c""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "a;b;c");
}

#[tokio::test]
async fn test_pipe_in_string() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"echo "a|b|c""#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "a|b|c");
}

// =============================================================================
// Arithmetic Stress
// =============================================================================

#[tokio::test]
async fn test_complex_arithmetic() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((1+2*3-4/2+5%3))").await.unwrap();
    // 1 + 6 - 2 + 2 = 7
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "7");
}

#[tokio::test]
async fn test_arithmetic_with_parens() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $(((1+2)*(3+4)))").await.unwrap();
    // 3 * 7 = 21
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "21");
}

#[tokio::test]
async fn test_negative_result() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo $((5 - 10))").await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "-5");
}

#[tokio::test]
async fn test_arithmetic_underflow() {
    let client = create_client().await.unwrap();
    // i64 min is -9223372036854775808
    let result = execute(&client, "echo $((-9223372036854775808 - 1))").await.unwrap();
    println!("Underflow: {:?}", result);
}

// =============================================================================
// Control Flow Stress
// =============================================================================

#[tokio::test]
async fn test_nested_if() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"
        if [[ 1 -eq 1 ]]; then
            if [[ 2 -eq 2 ]]; then
                if [[ 3 -eq 3 ]]; then
                    echo "deep"
                fi
            fi
        fi
    "#).await.unwrap();
    assert!(result.ok);
    assert_eq!(result.stdout.trim(), "deep");
}

#[tokio::test]
async fn test_nested_loops() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"
        for I in 1 2; do
            for J in a b; do
                echo "${I}${J}"
            done
        done
    "#).await.unwrap();
    assert!(result.ok);
    let lines: Vec<_> = result.stdout.trim().lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(lines, vec!["1a", "1b", "2a", "2b"]);
}

#[tokio::test]
async fn test_break_in_loop() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"
        for I in 1 2 3 4 5; do
            if [[ ${I} -eq 3 ]]; then
                break
            fi
            echo ${I}
        done
    "#).await.unwrap();
    assert!(result.ok);
    let lines: Vec<_> = result.stdout.trim().lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(lines, vec!["1", "2"]);
}

#[tokio::test]
async fn test_continue_in_loop() {
    let client = create_client().await.unwrap();
    let result = execute(&client, r#"
        for I in 1 2 3 4 5; do
            if [[ ${I} -eq 3 ]]; then
                continue
            fi
            echo ${I}
        done
    "#).await.unwrap();
    assert!(result.ok);
    let lines: Vec<_> = result.stdout.trim().lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(lines, vec!["1", "2", "4", "5"]);
}

// =============================================================================
// Rapid Fire
// =============================================================================

#[tokio::test]
async fn test_rapid_fire_100() {
    let client = create_client().await.unwrap();

    for i in 0..100 {
        let result = execute(&client, &format!("echo {}", i)).await.unwrap();
        assert!(result.ok, "Failed at iteration {}", i);
        assert_eq!(result.stdout.trim(), i.to_string());
    }
}

#[tokio::test]
async fn test_rapid_concurrent_50() {
    let client = create_client().await.unwrap();

    let handles: Vec<_> = (0..50)
        .map(|i| {
            let client = Arc::clone(&client);
            tokio::spawn(async move {
                let mut args = serde_json::Map::new();
                args.insert("script".into(), json!(format!("echo {}", i)));
                (i, client.call_tool("execute", Some(args)).await)
            })
        })
        .collect();

    for handle in handles {
        let (i, result) = handle.await.unwrap();
        assert!(result.is_ok(), "Failed at concurrent {}: {:?}", i, result);
    }
}

// =============================================================================
// Edge Cases That Might Crash
// =============================================================================

#[tokio::test]
async fn test_very_deep_nesting() {
    let client = create_client().await.unwrap();

    // 50 nested ifs - might hit recursion limits
    let mut script = String::new();
    for _ in 0..50 {
        script.push_str("if [[ 1 -eq 1 ]]; then ");
    }
    script.push_str("echo deep");
    for _ in 0..50 {
        script.push_str("; fi");
    }

    let result = execute(&client, &script).await.unwrap();
    println!("Very deep nesting: ok={}, stderr={}", result.ok, result.stderr);
}

#[tokio::test]
async fn test_script_with_nullish_json() {
    let client = create_client().await.unwrap();
    // Output that looks like JSON null
    let result = execute(&client, "echo null").await.unwrap();
    assert!(result.ok);
    // The data field should parse this as JSON null
    println!("Null JSON: data={:?}", result.data);
}

#[tokio::test]
async fn test_boolean_json() {
    let client = create_client().await.unwrap();
    let result = execute(&client, "echo true").await.unwrap();
    assert!(result.ok);
    println!("True JSON: data={:?}", result.data);

    let result = execute(&client, "echo false").await.unwrap();
    assert!(result.ok);
    println!("False JSON: data={:?}", result.data);
}
