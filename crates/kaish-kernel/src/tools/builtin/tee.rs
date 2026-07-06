//! tee — Read from stdin and write to both stdout and files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::backend::WriteMode;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tee tool: duplicate stdin to stdout and files.
pub struct Tee;

/// clap-derived argv layer for tee.
#[derive(Parser, Debug)]
#[command(name = "tee", about = "Read from stdin and write to stdout and files")]
struct TeeArgs {
    /// Append to file instead of overwriting.
    #[arg(id = "append", short = 'a', long = "append")]
    _append: bool,

    /// Confirmation nonce for a latch-gated overwrite.
    #[arg(long = "confirm")]
    confirm: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to write to in addition to stdout.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Tee {
    fn name(&self) -> &str {
        "tee"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TeeArgs::command(),
            "tee",
            "Read from stdin and write to stdout and files",
            [
                ("Save and display", "echo hello | tee output.txt"),
                ("Append to log", "echo entry | tee -a log.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match TeeArgs::try_parse_from(
            std::iter::once("tee".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tee: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "tee: missing file argument");
        }

        let append = args.has_flag("append") || args.has_flag("a");

        // Resolve every operand to a path once, up front — binary goes loud
        // rather than becoming a file literally named `[binary: N bytes]`.
        let paths = match crate::interpreter::values_to_text_sink_named(&args.positional, "a path") {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("tee: {e}")),
        };

        // Gate truncating overwrites through latch + trash (no-op when both are
        // off). Append never gates (it doesn't destroy prior content); a new
        // file just writes. On latch this returns an exit-2 nonce result; under
        // trash the prior content is snapshotted before we write below.
        let targets: Vec<(String, bool)> = paths.iter().map(|p| (p.clone(), append)).collect();
        let snapshots = match ctx
            .gate_overwrites("tee", &targets, parsed.confirm.as_deref(), |nonce, joined| {
                format!("tee --confirm=\"{nonce}\" {joined}")
            })
            .await
        {
            Ok(s) => s,
            Err(blocked) => return blocked,
        };

        // Read raw bytes so binary passes through tee intact (to files and to
        // the next stage).
        let input = ctx.read_stdin_to_bytes().await.unwrap_or_default();

        // POSIX: tee writes stdin to every file AND to stdout. Continue past
        // per-file errors (matches POSIX `tee` semantics) and report every
        // failure so the agent sees the full picture, not just the last one.
        let mut errors: Vec<String> = Vec::new();
        for path_str in &paths {
            let resolved = ctx.resolve_path(path_str);
            let path = Path::new(&resolved);

            // Overwrite writes the borrowed input directly (no clone); append
            // needs a read-modify-write since the VFS lacks an append mode. A
            // truncating overwrite of a gated target goes through a CAS against
            // the gate's snapshot, so a concurrent change between the gate and
            // this write is a loud conflict, not a silent clobber.
            let write_result = if append {
                let mut combined = ctx.backend.read(path, None).await.unwrap_or_default();
                combined.extend_from_slice(&input);
                ctx.backend
                    .write(path, &combined, WriteMode::Overwrite)
                    .await
                    .map_err(|e| e.to_string())
            } else {
                let expected = snapshots.get(&resolved).map(|v| v.as_slice());
                ctx.overwrite_checked(path, &input, expected).await
            };

            if let Err(e) = write_result {
                errors.push(format!("tee: {}: {}", path_str, e));
            }
        }

        // Pass the input through unchanged: text for text input, binary for
        // binary input.
        let mut result = ExecResult::success_text_or_bytes(input);
        if !errors.is_empty() {
            result.err = errors.join("\n");
            result = result.with_code(1);
        }
        result
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
        mem.write(Path::new("existing.txt"), b"original content\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tee_new_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/output.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello world\n");

        let written = ctx
            .backend
            .read(Path::new("/output.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"hello world\n");
    }

    #[tokio::test]
    async fn test_tee_overwrite() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("new content\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"new content\n");
    }

    #[tokio::test]
    async fn test_tee_append() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("appended\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));
        args.flags.insert("a".to_string());

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"original content\nappended\n");
    }

    #[tokio::test]
    async fn test_tee_empty_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/empty.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "");

        let written = ctx
            .backend
            .read(Path::new("/empty.txt"), None)
            .await
            .unwrap();
        assert!(written.is_empty());
    }

    #[tokio::test]
    async fn test_tee_missing_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("data\n".to_string());

        let result = Tee.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    // ── CAS overwrite (the primitive tee routes its truncating writes through) ──

    #[tokio::test]
    async fn overwrite_checked_rejects_concurrent_change() {
        let ctx = make_ctx().await; // /existing.txt = "original content\n"
        let path = Path::new("/existing.txt");
        let snapshot = b"original content\n".to_vec();

        // A concurrent writer changes the file after the gate snapshotted it.
        ctx.backend
            .write(path, b"changed elsewhere\n", WriteMode::Overwrite)
            .await
            .unwrap();

        // CAS against the now-stale snapshot must refuse to clobber.
        let result = ctx.overwrite_checked(path, b"my content\n", Some(&snapshot)).await;
        assert!(result.is_err(), "expected a conflict, got {result:?}");

        // The concurrent writer's content survives untouched.
        let now = ctx.backend.read(path, None).await.unwrap();
        assert_eq!(now, b"changed elsewhere\n");
    }

    #[tokio::test]
    async fn overwrite_checked_writes_when_snapshot_matches() {
        let ctx = make_ctx().await;
        let path = Path::new("/existing.txt");
        let snapshot = b"original content\n".to_vec();

        ctx.overwrite_checked(path, b"new content\n", Some(&snapshot))
            .await
            .unwrap();
        assert_eq!(
            ctx.backend.read(path, None).await.unwrap(),
            b"new content\n"
        );
    }

    #[tokio::test]
    async fn overwrite_checked_skips_cas_without_expectation() {
        let ctx = make_ctx().await;
        let path = Path::new("/existing.txt");

        // No snapshot (gate off / new file): a plain overwrite, no CAS.
        ctx.overwrite_checked(path, b"forced\n", None).await.unwrap();
        assert_eq!(ctx.backend.read(path, None).await.unwrap(), b"forced\n");
    }

    #[tokio::test]
    async fn overwrite_checked_errors_when_reread_fails_even_for_empty_snapshot() {
        let ctx = make_ctx().await;
        // Snapshot an EMPTY file, then have it vanish (concurrent delete).
        let path = Path::new("/empty.txt");
        ctx.backend.write(path, b"", WriteMode::Overwrite).await.unwrap();
        let empty_snapshot: Vec<u8> = Vec::new();
        ctx.backend.remove(path, false).await.unwrap();

        // A failed re-read must surface loudly — not be swallowed to `[]` and
        // false-match the empty snapshot, which would silently (re)write.
        let result = ctx
            .overwrite_checked(path, b"new\n", Some(&empty_snapshot))
            .await;
        assert!(result.is_err(), "vanished target must error, got {result:?}");
    }
}
