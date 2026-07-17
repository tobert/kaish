//! base64 — Encode or decode base64 data.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use base64::engine::general_purpose::STANDARD;
use base64::Engine;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Base64 tool: encode or decode base64 data.
pub struct Base64Tool;

/// clap-derived argv layer for base64.
#[derive(Parser, Debug)]
#[command(name = "base64", about = "Encode or decode base64 data")]
struct Base64Args {
    /// Decode base64 input (-d)
    #[arg(short = 'd', long = "decode")]
    decode: bool,

    /// Wrap encoded output at column N (0 = no wrap) (-w)
    #[arg(short = 'w', long = "wrap")]
    wrap: Option<i64>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Input file; reads stdin when omitted.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Base64Tool {
    fn name(&self) -> &str {
        "base64"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &Base64Args::command(),
            "base64",
            "Encode or decode base64 data",
            [
                ("Encode stdin", "echo hello | base64"),
                ("Decode", "echo aGVsbG8K | base64 -d"),
                ("Encode without wrapping", "base64 -w 0 file.bin"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Tests poke `args.named.insert("decode", Value::Bool(true))` directly;
        // to_argv would render that as `--decode=true` which clap won't accept
        // for a bool field. Promote any Bool-typed named entries to flags so
        // they hit the clap struct via the natural short/long form.
        args.flagify_bool_named(&self.schema());

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("base64: {e}")),
        };
        let parsed = match Base64Args::try_parse_from(
            std::iter::once("base64".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("base64: {e}")),
        };
        parsed.global.apply(ctx);

        let decode = parsed.decode;
        let wrap_col = parsed
            .wrap
            .map(|n| n as usize)
            .or_else(|| {
                args.get("wrap", usize::MAX).and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
            })
            .unwrap_or(76);

        // Get input from file(s) or stdin, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("base64: {}", e)),
        };

        // Read input as raw bytes — base64 is fundamentally a binary codec, so
        // never decode the input as UTF-8 (that would reject/mangle binary files).
        let data: Vec<u8> = match paths.first() {
            Some(path) => {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(d) => d,
                    Err(e) => return ExecResult::failure(1, format!("base64: {}: {}", path, e)),
                }
            }
            None => ctx.read_stdin_to_bytes().await.unwrap_or_default(),
        };

        if decode {
            // Strip whitespace before decoding (base64 input often has newlines).
            let cleaned: Vec<u8> = data.iter().copied().filter(|b| !b.is_ascii_whitespace()).collect();
            match STANDARD.decode(&cleaned) {
                // Decoded bytes: text if valid UTF-8, otherwise a binary result.
                Ok(bytes) => ExecResult::success_text_or_bytes(bytes),
                Err(e) => ExecResult::failure(1, format!("base64: invalid input: {}", e)),
            }
        } else {
            // Encode: raw input bytes → base64 text.
            let encoded = STANDARD.encode(&data);
            let mut output = if wrap_col > 0 {
                wrap_lines(&encoded, wrap_col)
            } else {
                encoded
            };
            // Trailing-newline policy (builtin-sweep P4.1): encoded output is
            // newline-terminated, like GNU base64. (Decode stays raw — above.)
            if !output.is_empty() {
                output.push('\n');
            }
            ExecResult::with_output(OutputData::text(output))
        }
    }
}

/// Wrap a string at the given column width.
fn wrap_lines(s: &str, width: usize) -> String {
    let mut result = String::with_capacity(s.len() + s.len() / width);
    for (i, ch) in s.chars().enumerate() {
        if i > 0 && i % width == 0 {
            result.push('\n');
        }
        result.push(ch);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("hello.txt"), b"hello")
            .await
            .expect("write failed");
        mem.write(Path::new("encoded.txt"), b"aGVsbG8=")
            .await
            .expect("write failed");
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_encode_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let args = ToolArgs::new();
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "aGVsbG8=\n");
    }

    #[tokio::test]
    async fn test_decode_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("aGVsbG8=".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("decode".to_string(), Value::Bool(true));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "hello");
    }

    #[tokio::test]
    async fn test_encode_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/hello.txt".into()));

        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "aGVsbG8=\n");
    }

    #[tokio::test]
    async fn test_decode_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/encoded.txt".into()));
        args.named
            .insert("decode".to_string(), Value::Bool(true));

        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "hello");
    }

    #[tokio::test]
    async fn test_encode_wrap() {
        let mut ctx = make_ctx().await;
        // Long input to trigger wrapping
        ctx.set_stdin("The quick brown fox jumps over the lazy dog".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("wrap".to_string(), Value::Int(20));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Each line should be at most 20 chars
        for line in result.text_out().as_ref().lines() {
            assert!(line.len() <= 20, "line too long: {}", line);
        }
    }

    #[tokio::test]
    async fn test_encode_no_wrap() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("The quick brown fox jumps over the lazy dog".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("wrap".to_string(), Value::Int(0));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!result.text_out().trim_end_matches('\n').contains('\n'));
    }

    #[tokio::test]
    async fn test_decode_invalid() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("not-valid-base64!!!".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("decode".to_string(), Value::Bool(true));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_roundtrip() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("roundtrip test 日本語".to_string());

        let args = ToolArgs::new();
        let encoded = Base64Tool.execute(args, &mut ctx).await;
        assert!(encoded.ok());

        ctx.set_stdin(encoded.text_out().into_owned());
        let mut args2 = ToolArgs::new();
        args2
            .named
            .insert("decode".to_string(), Value::Bool(true));
        let decoded = Base64Tool.execute(args2, &mut ctx).await;
        assert!(decoded.ok());
        assert_eq!(decoded.text_out().as_ref(), "roundtrip test 日本語");
    }

    #[tokio::test]
    async fn test_decode_with_whitespace() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("aGVs\nbG8=\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("decode".to_string(), Value::Bool(true));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "hello");
    }

    #[tokio::test]
    async fn test_decode_to_binary_yields_bytes() {
        // "/wA=" decodes to [0xFF, 0x00] — not valid UTF-8. The result must be
        // a Bytes payload, not a lossy-mangled string.
        let mut ctx = make_ctx().await;
        ctx.set_stdin("/wA=".to_string());
        let mut args = ToolArgs::new();
        args.named.insert("decode".to_string(), Value::Bool(true));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.is_bytes(), "binary decode must yield a Bytes result");
        assert_eq!(result.out_bytes(), Some(&[0xffu8, 0x00][..]));
    }

    #[tokio::test]
    async fn test_encode_binary_file() {
        // Encoding must read the file as raw bytes, not reject non-UTF-8.
        let mut ctx = make_ctx().await;
        ctx.backend
            .write(Path::new("/blob.bin"), &[0u8, 0xff, 0x10], crate::backend::WriteMode::Overwrite)
            .await
            .unwrap();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok(), "stderr: {}", result.err);
        assert_eq!(result.text_out().as_ref(), format!("{}\n", STANDARD.encode([0u8, 0xff, 0x10])));
    }

    #[tokio::test]
    async fn test_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Base64Tool.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "");
    }
}
