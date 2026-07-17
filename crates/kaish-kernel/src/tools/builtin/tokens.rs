//! tokens — Count BPE tokens using tiktoken.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use tiktoken_rs::{cl100k_base, o200k_base, p50k_base};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tokens tool: count BPE tokens in text.
pub struct Tokens;

/// clap-derived argv layer for tokens.
#[derive(Parser, Debug)]
#[command(name = "tokens", about = "Count BPE tokens using tiktoken tokenization")]
struct TokensArgs {
    /// Encoding: cl100k (Claude/GPT-4), o200k (GPT-4o), p50k (GPT-3)
    #[arg(short = 'e', long = "encoding")]
    encoding: Option<String>,

    /// Show token IDs (-v)
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Text to count tokens for; reads stdin when omitted.
    text: Vec<String>,
}

#[async_trait]
impl Tool for Tokens {
    fn name(&self) -> &str {
        "tokens"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TokensArgs::command(),
            "tokens",
            "Count BPE tokens using tiktoken tokenization",
            [
                ("Count tokens (default: cl100k)", "tokens \"Hello, Kaijutsu! 会術\""),
                ("From stdin", "cat file.txt | tokens"),
                ("Verbose (show token IDs)", "tokens -v \"Hello\""),
                ("Different encoding", "tokens -e o200k \"Hello\""),
                ("JSON output", "tokens --json \"Hello world\""),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("tokens: {e}")),
        };
        let parsed = match TokensArgs::try_parse_from(
            std::iter::once("tokens".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tokens: {e}")),
        };
        parsed.global.apply(ctx);

        // Get text from positional arg or stdin
        let text = match args.get_string("text", 0) {
            Some(t) => t,
            None => match ctx.read_stdin_to_text().await {
                Ok(Some(s)) => s,
                Ok(None) => return ExecResult::failure(1, "tokens: no input provided"),
                Err(e) => return ExecResult::failure(2, format!("tokens: {e}")),
            },
        };

        // Get encoding (named or -e)
        let encoding = parsed.encoding.clone()
            .or_else(|| args.get_string("encoding", usize::MAX))
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

        // Build structured output: full table of token IDs.
        // Default pipe output is just the count (for `cat file | tokens`),
        // but --json gets the full table via OutputData.
        let verbose = parsed.verbose;

        if verbose {
            let mut lines = format!("count: {}\nids: [", count);
            for (i, id) in token_ids.iter().enumerate() {
                if i > 0 { lines.push_str(", "); }
                lines.push_str(&id.to_string());
            }
            lines.push(']');
            return ExecResult::with_output(OutputData::text(lines));
        }

        let nodes: Vec<OutputNode> = token_ids
            .iter()
            .enumerate()
            .map(|(i, id)| OutputNode::new(i.to_string()).with_cells(vec![id.to_string()]))
            .collect();
        let table = OutputData::table(
            vec!["INDEX".to_string(), "ID".to_string()],
            nodes,
        );

        // Pipe output is just the count; structured data carries the full table
        ExecResult::with_output_and_text(table, count.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
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
        assert_eq!(result.text_out().trim(), "2");
    }

    #[tokio::test]
    async fn test_tokens_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("Hello world".to_string());

        let args = ToolArgs::new();
        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "2");
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
        assert!(result.text_out().contains("count:"));
        assert!(result.text_out().contains("ids:"));
    }

    #[tokio::test]
    async fn test_tokens_json_via_global_flag() {
        use crate::interpreter::{apply_output_format, OutputFormat};

        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Default pipe output is just the count
        assert_eq!(result.text_out().trim(), "1");
        // But OutputData carries the full table
        assert!(result.has_output());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value = serde_json::from_str(&result.text_out()).expect("valid JSON");
        // Should be an array of {INDEX, ID} objects
        let arr = json.as_array().expect("should be array");
        assert_eq!(arr.len(), 1); // "Hello" is 1 token in cl100k
        assert!(arr[0].get("INDEX").is_some());
        assert!(arr[0].get("ID").is_some());
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
        let count: usize = result.text_out().trim().parse().expect("valid number");
        assert!(count >= 1);
    }

    #[tokio::test]
    async fn test_tokens_empty_string() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("".into()));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "0");
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
        let count: usize = result.text_out().trim().parse().expect("valid number");
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
        let count: usize = result.text_out().trim().parse().expect("valid number");
        assert!(count >= 1);
    }

    #[tokio::test]
    async fn test_tokens_long_text() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        // Create a longer text to test
        let text = "The quick brown fox jumps over the lazy dog. ".repeat(10);
        args.positional.push(Value::String(text));

        let result = Tokens.execute(args, &mut ctx).await;
        assert!(result.ok());
        let count: usize = result.text_out().trim().parse().expect("valid number");
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
        let count: usize = result.text_out().trim().parse().expect("valid number");
        assert!(count >= 1);
    }
}
