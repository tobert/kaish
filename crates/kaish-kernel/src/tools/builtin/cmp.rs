//! cmp — compare two files byte by byte.
//!
//! The binary-aware sibling of `diff`: reads both operands as raw bytes (never
//! UTF-8), so it works on any content. Exit status follows POSIX `cmp`:
//! 0 = identical, 1 = differ, 2 = error. With `-s` it is silent (status only).
//! See `docs/binary-data.md`.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// cmp tool.
pub struct Cmp;

/// clap-derived argv layer for cmp. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "cmp", about = "Compare two files byte by byte")]
struct CmpArgs {
    /// Silent — no output, only the exit status (-s).
    #[arg(short = 's', long = "silent")]
    silent: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// The two files to compare (use `-` for stdin in one slot).
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Cmp {
    fn name(&self) -> &str {
        "cmp"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CmpArgs::command(),
            "cmp",
            "Compare two files byte by byte",
            [
                ("Compare two files", "cmp a.bin b.bin"),
                ("Silent (exit status only)", "cmp -s a.bin b.bin"),
                ("Compare stdin to a file", "dd if=/dev/urandom bs=16 count=1 | cmp - saved.bin"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named();
        let parsed = match CmpArgs::try_parse_from(
            std::iter::once("cmp".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cmp: {e}")),
        };
        parsed.global.apply(ctx);
        let silent = parsed.silent;

        if parsed.paths.len() != 2 {
            return ExecResult::failure(2, "cmp: expected two file operands");
        }
        let (name1, name2) = (parsed.paths[0].clone(), parsed.paths[1].clone());
        if name1 == "-" && name2 == "-" {
            return ExecResult::failure(2, "cmp: only one operand may be '-' (stdin)");
        }

        let a = match read_operand(ctx, &name1).await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name1}: {e}")),
        };
        let b = match read_operand(ctx, &name2).await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name2}: {e}")),
        };

        // First differing byte (1-based), with its line number (newlines before it + 1).
        let common = a.len().min(b.len());
        if let Some(offset) = (0..common).find(|&i| a[i] != b[i]) {
            if silent {
                return ExecResult::failure(1, "");
            }
            let line = a[..offset].iter().filter(|&&c| c == b'\n').count() + 1;
            let msg = format!("{name1} {name2} differ: byte {}, line {}", offset + 1, line);
            return ExecResult::success(msg).with_code(1);
        }

        // Common prefix matches; a length difference is EOF on the shorter file.
        if a.len() != b.len() {
            if silent {
                return ExecResult::failure(1, "");
            }
            let (shorter, at) = if a.len() < b.len() {
                (&name1, a.len())
            } else {
                (&name2, b.len())
            };
            let msg = format!("cmp: EOF on {shorter} after byte {at}");
            return ExecResult::success(msg).with_code(1);
        }

        // Identical → exit 0, no output.
        ExecResult::success("")
    }
}

/// Read an operand as raw bytes — a path through the VFS, or stdin for `-`.
async fn read_operand(ctx: &mut ExecContext, name: &str) -> std::io::Result<Vec<u8>> {
    if name == "-" {
        Ok(ctx.read_stdin_to_bytes().await.unwrap_or_default())
    } else {
        let resolved = ctx.resolve_path(name);
        ctx.backend
            .read(Path::new(&resolved), None)
            .await
            .map_err(std::io::Error::other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("a.bin"), &[0u8, 1, 2, 3]).await.unwrap();
        mem.write(Path::new("a2.bin"), &[0u8, 1, 2, 3]).await.unwrap();
        mem.write(Path::new("b.bin"), &[0u8, 1, 9, 3]).await.unwrap();
        mem.write(Path::new("short.bin"), &[0u8, 1]).await.unwrap();
        mem.write(Path::new("lines.txt"), b"ab\ncd").await.unwrap();
        mem.write(Path::new("lines2.txt"), b"ab\ncX").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    fn args(ops: &[&str]) -> ToolArgs {
        let mut a = ToolArgs::new();
        for o in ops {
            a.positional.push(Value::String((*o).to_string()));
        }
        a
    }

    #[tokio::test]
    async fn identical_files_exit_zero() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/a2.bin"]), &mut ctx).await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.text_out().is_empty());
    }

    #[tokio::test]
    async fn differing_byte_reported() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/b.bin"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        // First difference is at byte index 2 → byte 3, line 1.
        assert!(r.text_out().contains("byte 3, line 1"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn line_number_counts_newlines() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/lines.txt", "/lines2.txt"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        // Differ at the 5th byte ('d' vs 'X'), which is on line 2.
        assert!(r.text_out().contains("byte 5, line 2"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn eof_on_shorter() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/short.bin", "/a.bin"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        assert!(r.text_out().contains("EOF on /short.bin"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn silent_suppresses_output() {
        let mut ctx = make_ctx().await;
        let mut a = args(&["/a.bin", "/b.bin"]);
        a.named.insert("silent".to_string(), Value::Bool(true));
        let r = Cmp.execute(a, &mut ctx).await;
        assert_eq!(r.code, 1);
        assert!(r.text_out().is_empty());
        assert!(r.err.is_empty());
    }

    #[tokio::test]
    async fn missing_operand_errors() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin"]), &mut ctx).await;
        assert_eq!(r.code, 2);
    }

    #[tokio::test]
    async fn missing_file_is_error_exit_2() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/nope.bin"]), &mut ctx).await;
        assert_eq!(r.code, 2);
    }
}
