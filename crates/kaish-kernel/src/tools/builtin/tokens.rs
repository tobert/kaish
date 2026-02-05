//! tokens — Count BPE tokens using tiktoken.

use async_trait::async_trait;
use tiktoken_rs::{cl100k_base, o200k_base, p50k_base};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Helper to extract string from Value.
fn value_as_string(v: &Value) -> Option<String> {
    match v {
        Value::String(s) => Some(s.clone()),
        _ => None,
    }
}

/// Tokens tool: count BPE tokens in text.
pub struct Tokens;

#[async_trait]
impl Tool for Tokens {
    fn name(&self) -> &str {
        "tokens"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tokens", "Count BPE tokens using tiktoken tokenization")
            .param(ParamSchema::optional(
                "text",
                "string",
                Value::Null,
                "Text to tokenize (reads stdin if not provided)",
            ))
            .param(
                ParamSchema::optional(
                    "encoding",
                    "string",
                    Value::String("cl100k".into()),
                    "Encoding: cl100k (Claude/GPT-4), o200k (GPT-4o), p50k (GPT-3)",
                )
                .with_aliases(["-e"]),
            )
            .param(
                ParamSchema::optional(
                    "verbose",
                    "bool",
                    Value::Bool(false),
                    "Show token IDs (-v)",
                )
                .with_aliases(["-v"]),
            )
            .param(
                ParamSchema::optional("json", "bool", Value::Bool(false), "JSON output (-j)")
                    .with_aliases(["-j"]),
            )
            .example("Count tokens (default: cl100k)", "tokens \"Hello, Kaijutsu! 会術\"")
            .example("From stdin", "cat file.txt | tokens")
            .example("Verbose (show token IDs)", "tokens -v \"Hello\"")
            .example("Different encoding", "tokens -e o200k \"Hello\"")
            .example("JSON output", "tokens -j \"Hello world\"")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get text from positional arg or stdin
        let text = match args.get_string("text", 0) {
            Some(t) => t,
            None => match ctx.take_stdin() {
                Some(s) => s,
                None => return ExecResult::failure(1, "tokens: no input provided"),
            },
        };

        // Get encoding (check both named and -e alias via positional)
        let encoding = args
            .get_string("encoding", usize::MAX)
            .or_else(|| args.named.get("e").and_then(value_as_string))
            .unwrap_or_else(|| "cl100k".into());

        // Get the BPE encoder
        let bpe = match encoding.as_str() {
            "cl100k" | "cl100k_base" => match cl100k_base() {
                Ok(b) => b,
                Err(e) => return ExecResult::failure(1, format!("tokens: failed to load cl100k encoding: {}", e)),
            },
            "o200k" | "o200k_base" => match o200k_base() {
                Ok(b) => b,
                Err(e) => return ExecResult::failure(1, format!("tokens: failed to load o200k encoding: {}", e)),
            },
            "p50k" | "p50k_base" => match p50k_base() {
                Ok(b) => b,
                Err(e) => return ExecResult::failure(1, format!("tokens: failed to load p50k encoding: {}", e)),
            },
            _ => {
                return ExecResult::failure(
                    1,
                    format!("tokens: unknown encoding '{}' (use cl100k, o200k, or p50k)", encoding),
                )
            }
        };

        // Tokenize
        let token_ids = bpe.encode_with_special_tokens(&text);
        let count = token_ids.len();

        // Output format
        let json_output = args.has_flag("json") || args.has_flag("j");
        let verbose = args.has_flag("verbose") || args.has_flag("v");

        if json_output {
            let json = serde_json::json!({
                "count": count,
                "ids": token_ids
            });
            return ExecResult::success_data(Value::Json(json));
        }

        if verbose {
            return ExecResult::success(format!("count: {}\nids: {:?}", count, token_ids));
        }

        ExecResult::success(count.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tokens_basic() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello world".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        // cl100k encodes "Hello world" as 2 tokens
        assert_eq!(result.out.trim(), "2");
    }

    #[tokio::test]
    async fn test_tokens_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("Hello world".to_string());

        let args = ToolArgs::new();
        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "2");
    }

    #[tokio::test]
    async fn test_tokens_no_input() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[tokio::test]
    async fn test_tokens_verbose() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello".into()));
        args.flags.insert("v".to_string());

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("count:"));
        assert!(result.out.contains("ids:"));
    }

    #[tokio::test]
    async fn test_tokens_json() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello".into()));
        args.flags.insert("j".to_string());

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Parse the JSON output
        let json: serde_json::Value = serde_json::from_str(&result.out).expect("valid JSON");
        assert!(json.get("count").is_some());
        assert!(json.get("ids").is_some());
    }

    #[tokio::test]
    async fn test_tokens_unicode() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        // Japanese text - should tokenize to multiple tokens
        args.positional.push(Value::String("会術".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should have at least 1 token
        let count: usize = result.out.trim().parse().expect("valid number");
        assert!(count >= 1);
    }

    #[tokio::test]
    async fn test_tokens_empty_string() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "0");
    }

    #[tokio::test]
    async fn test_tokens_unknown_encoding() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello".into()));
        args.named
            .insert("encoding".to_string(), Value::String("invalid".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("unknown encoding"));
    }

    #[tokio::test]
    async fn test_tokens_o200k_encoding() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello world".into()));
        args.named
            .insert("encoding".to_string(), Value::String("o200k".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        // o200k also encodes "Hello world" but may have different count
        let count: usize = result.out.trim().parse().expect("valid number");
        assert!(count >= 1);
    }

    #[tokio::test]
    async fn test_tokens_p50k_encoding() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello world".into()));
        args.named
            .insert("encoding".to_string(), Value::String("p50k".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        let count: usize = result.out.trim().parse().expect("valid number");
        assert!(count >= 1);
    }

    #[tokio::test]
    async fn test_tokens_long_text() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        // Create a longer text to test
        let text = "The quick brown fox jumps over the lazy dog. ".repeat(10);
        args.positional.push(Value::String(text.into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        let count: usize = result.out.trim().parse().expect("valid number");
        // Should have many tokens
        assert!(count > 10);
    }

    #[tokio::test]
    async fn test_tokens_special_characters() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("Hello\nWorld\t!@#$%".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        let count: usize = result.out.trim().parse().expect("valid number");
        assert!(count >= 1);
    }
}
