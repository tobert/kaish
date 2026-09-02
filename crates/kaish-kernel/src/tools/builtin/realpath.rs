//! realpath — Print the resolved absolute pathname.
//!
//! Canonicalizes through symlinks via the VFS backend. Errors on nonexistent
//! paths (GNU realpath default; `-m`/`-e` variants not yet implemented).

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Realpath tool: resolve path to absolute, canonical form.
pub struct Realpath;

/// clap-derived argv layer for realpath.
#[derive(Parser, Debug)]
#[command(name = "realpath", about = "Print the resolved absolute pathname")]
struct RealpathArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Paths to canonicalize.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Realpath {
    fn name(&self) -> &str {
        "realpath"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &RealpathArgs::command(),
            "realpath",
            "Print the resolved absolute pathname",
            [
                ("Resolve a path", "realpath ../lib"),
                ("Normalize path", "realpath /usr/bin/../lib"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("realpath: {e}")),
        };
        let parsed = match RealpathArgs::try_parse_from(
            std::iter::once("realpath".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("realpath: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "realpath: missing path argument");
        }

        // GNU realpath (no flags): canonicalize each path fully; all components
        // including the final one must exist. Errors are printed but we continue
        // processing remaining paths (exit code 1 at end if any failed).
        let mut output = String::new();
        let mut exit_code = 0i64;
        let mut last_err: Option<String> = None;

        for value in &args.positional {
            let path_str = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("realpath: {e}")),
            };
            let resolved = ctx.resolve_path(&path_str);

            // GNU realpath (no -m): every component, including the final
            // one, must exist — `allow_missing_final: false`.
            match ctx.backend.canonicalize(std::path::Path::new(&resolved), false).await {
                Ok(canonical) => {
                    output.push_str(&canonical.to_string_lossy());
                    output.push('\n');
                }
                Err(e) => {
                    last_err = Some(format!("realpath: {}: {}", path_str, e));
                    exit_code = 1;
                }
            }
        }

        let mut result = ExecResult::with_output(OutputData::text(output));
        if let Some(msg) = last_err {
            result.err = ExecResult::terminate_diagnostic(msg);
            result = result.with_code(exit_code);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_realpath_absolute_existing() {
        use crate::vfs::Filesystem;
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(std::path::Path::new("bin/sort"), b"").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/bin/sort".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok(), "err: {}", result.err);
        assert_eq!(result.text_out().trim(), "/bin/sort");
    }

    #[tokio::test]
    async fn test_realpath_with_dotdot() {
        use crate::vfs::Filesystem;
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(std::path::Path::new("usr/lib/libfoo.so"), b"").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/usr/bin/../lib/libfoo.so".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok(), "err: {}", result.err);
        assert_eq!(result.text_out().trim(), "/usr/lib/libfoo.so");
    }

    #[tokio::test]
    async fn test_realpath_nonexistent_fails() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure for nonexistent path");
    }

    #[tokio::test]
    async fn test_realpath_missing_path() {
        let mut ctx = make_ctx();
        let result = Realpath.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
