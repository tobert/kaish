//! xxd — Make a hex dump or reverse it.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Xxd tool: hex dump or reverse.
pub struct Xxd;

/// clap-derived argv layer for xxd. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "xxd", about = "Make a hex dump or reverse it")]
struct XxdArgs {
    /// Plain hex dump — no address, no ASCII (-p)
    #[arg(short = 'p', long = "plain")]
    plain: bool,

    /// Reverse: convert hex dump back to binary (-r)
    #[arg(short = 'r', long = "reverse")]
    reverse: bool,

    /// Limit output to N bytes (-l)
    #[arg(short = 'l', long = "length")]
    length: Option<i64>,

    /// Skip N bytes from start (-s)
    #[arg(short = 's', long = "seek")]
    seek: Option<i64>,

    #[command(flatten)]
    global: GlobalFlags,

    /// File to read; reads stdin when omitted.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Xxd {
    fn name(&self) -> &str {
        "xxd"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &XxdArgs::command(),
            "xxd",
            "Make a hex dump or reverse it",
            [
                ("Hex dump", "xxd file.bin"),
                ("Plain hex", "echo hello | xxd -p"),
                ("Reverse hex", "echo 68656c6c6f | xxd -r -p"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Tests poke args.named.insert("plain", Value::Bool(true)); to_argv would
        // produce `--plain=true` which clap rejects for a bool field. Promote
        // bool-typed named entries into flag form.
        args.flagify_bool_named(&self.schema());

        let parsed = match XxdArgs::try_parse_from(
            std::iter::once("xxd".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("xxd: {e}")),
        };
        parsed.global.apply(ctx);

        let plain = parsed.plain;
        let reverse = parsed.reverse;
        let length = parsed.length.map(|n| n as usize).or_else(|| {
            args.get("length", usize::MAX).and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
        });
        let seek = parsed
            .seek
            .map(|n| n as usize)
            .or_else(|| {
                args.get("seek", usize::MAX).and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
            })
            .unwrap_or(0);

        // Read raw input from file(s) or stdin, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("xxd: {}", e)),
        };

        // Read raw bytes — a hex dump of binary must see the real bytes, never
        // a lossy-decoded approximation.
        let data: Vec<u8> = match paths.first() {
            Some(path) => {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(d) => d,
                    Err(e) => return ExecResult::failure(1, format!("xxd: {}: {}", path, e)),
                }
            }
            None => ctx.read_stdin_to_bytes().await.unwrap_or_default(),
        };

        if reverse {
            // Hex input is ASCII; a lossy view is lossless here. Output is bytes.
            return reverse_hex(&String::from_utf8_lossy(&data), plain);
        }

        // Forward: produce hex dump from the raw bytes.
        let bytes = &data[..];

        // Apply seek
        let bytes = if seek < bytes.len() {
            &bytes[seek..]
        } else {
            &[][..]
        };

        // Apply length limit
        let bytes = match length {
            Some(n) if n < bytes.len() => &bytes[..n],
            _ => bytes,
        };

        let output = if plain {
            plain_hex(bytes)
        } else {
            classic_hex(bytes, seek)
        };

        ExecResult::with_output(OutputData::text(output))
    }
}

/// Classic xxd format: address, hex pairs, ASCII representation.
/// 16 bytes per line.
fn classic_hex(bytes: &[u8], base_offset: usize) -> String {
    let mut output = String::new();
    for (i, chunk) in bytes.chunks(16).enumerate() {
        let addr = base_offset + i * 16;

        // Address
        output.push_str(&format!("{:08x}: ", addr));

        // Hex pairs (groups of 2 bytes separated by space)
        for (j, byte) in chunk.iter().enumerate() {
            output.push_str(&format!("{:02x}", byte));
            if j % 2 == 1 {
                output.push(' ');
            }
        }

        // Pad if short line
        let hex_width = chunk.len() * 2 + chunk.len() / 2;
        let full_width = 16 * 2 + 8; // 32 hex chars + 8 spaces
        for _ in hex_width..full_width {
            output.push(' ');
        }

        // ASCII representation
        output.push(' ');
        for byte in chunk {
            if byte.is_ascii_graphic() || *byte == b' ' {
                output.push(*byte as char);
            } else {
                output.push('.');
            }
        }

        output.push('\n');
    }

    // Each line is newline-terminated, including the last (builtin-sweep P4.1).
    output
}

/// Plain hex: just hex bytes, no address or ASCII. 30 bytes per line.
fn plain_hex(bytes: &[u8]) -> String {
    let mut output = String::new();
    for (i, byte) in bytes.iter().enumerate() {
        output.push_str(&format!("{:02x}", byte));
        if i > 0 && (i + 1) % 30 == 0 {
            output.push('\n');
        }
    }
    // Terminate the final (partial) line too (builtin-sweep P4.1).
    if !output.is_empty() && !output.ends_with('\n') {
        output.push('\n');
    }
    output
}

/// Reverse: parse hex input back to text.
fn reverse_hex(input: &str, plain: bool) -> ExecResult {
    let hex_str = if plain {
        // Plain mode: input is just hex chars
        input
            .chars()
            .filter(|c| c.is_ascii_hexdigit())
            .collect::<String>()
    } else {
        // Classic mode: extract hex from xxd-format lines
        // Each line: "00000000: 6865 6c6c 6f0a       hello."
        // Take the hex portion between address and ASCII
        let mut hex = String::new();
        for line in input.lines() {
            // Skip empty lines
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            // After the colon+space, take hex pairs until we hit two spaces (ASCII section)
            if let Some(after_addr) = line.split(": ").nth(1) {
                // Take until double space (which separates hex from ASCII)
                let hex_part = after_addr.split("  ").next().unwrap_or("");
                for ch in hex_part.chars() {
                    if ch.is_ascii_hexdigit() {
                        hex.push(ch);
                    }
                }
            }
        }
        hex
    };

    // Convert hex string to bytes
    let mut bytes = Vec::with_capacity(hex_str.len() / 2);
    let chars: Vec<char> = hex_str.chars().collect();
    let mut i = 0;
    while i + 1 < chars.len() {
        let high = chars[i].to_digit(16);
        let low = chars[i + 1].to_digit(16);
        match (high, low) {
            (Some(h), Some(l)) => bytes.push((h * 16 + l) as u8),
            _ => {
                return ExecResult::failure(
                    1,
                    format!("xxd: invalid hex at position {}", i),
                )
            }
        }
        i += 2;
    }

    // Reversed bytes: text if valid UTF-8, otherwise a binary result.
    ExecResult::success_text_or_bytes(bytes)
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
    async fn test_xxd_classic() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let args = ToolArgs::new();
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().as_ref().starts_with("00000000:"));
        assert!(result.text_out().as_ref().contains("6865"));
        assert!(result.text_out().as_ref().contains("hello"));
    }

    #[tokio::test]
    async fn test_xxd_plain() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "68656c6c6f\n");
    }

    #[tokio::test]
    async fn test_xxd_reverse_plain() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("68656c6c6f".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("reverse".to_string(), Value::Bool(true));
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "hello");
    }

    #[tokio::test]
    async fn test_xxd_roundtrip() {
        let mut ctx = make_ctx().await;
        let original = "test data 123";
        ctx.set_stdin(original.to_string());

        // Forward
        let mut args = ToolArgs::new();
        args.named.insert("plain".to_string(), Value::Bool(true));
        let hex = Xxd.execute(args, &mut ctx).await;
        assert!(hex.ok());

        // Reverse
        ctx.set_stdin(hex.text_out().into_owned());
        let mut args2 = ToolArgs::new();
        args2
            .named
            .insert("reverse".to_string(), Value::Bool(true));
        args2.named.insert("plain".to_string(), Value::Bool(true));
        let decoded = Xxd.execute(args2, &mut ctx).await;
        assert!(decoded.ok());
        assert_eq!(decoded.text_out().as_ref(), original);
    }

    #[tokio::test]
    async fn test_xxd_seek() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("abcdef".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("seek".to_string(), Value::Int(3));
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "646566\n"); // "def" in hex
    }

    #[tokio::test]
    async fn test_xxd_length() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("abcdef".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("length".to_string(), Value::Int(3));
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "616263\n"); // "abc" in hex
    }

    #[tokio::test]
    async fn test_xxd_dumps_binary_file_exactly() {
        use crate::vfs::Filesystem;
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("blob.bin"), &[0xffu8, 0x00, 0x10]).await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok(), "stderr: {}", result.err);
        // Exact hex of the real bytes — not a lossy approximation.
        assert_eq!(result.text_out().as_ref(), "ff0010\n");
    }

    #[tokio::test]
    async fn test_xxd_reverse_to_binary_yields_bytes() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("ff00".to_string());
        let mut args = ToolArgs::new();
        args.named.insert("reverse".to_string(), Value::Bool(true));
        args.named.insert("plain".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.is_bytes(), "reverse to binary must yield Bytes");
        assert_eq!(result.out_bytes(), Some(&[0xffu8, 0x00][..]));
    }

    #[tokio::test]
    async fn test_xxd_empty() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "");
    }

    #[tokio::test]
    async fn test_xxd_reverse_classic_format() {
        let mut ctx = make_ctx().await;
        // Feed classic xxd output back to reverse
        ctx.set_stdin("00000000: 6865 6c6c 6f                             hello".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("reverse".to_string(), Value::Bool(true));
        let result = Xxd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "hello");
    }
}
