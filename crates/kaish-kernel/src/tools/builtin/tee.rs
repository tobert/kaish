//! tee — Read from stdin and write to both stdout and files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::ExecResult;
use crate::operation::KernelOperation;
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
        .with_operations([KernelOperation::FsOverwrite.as_str()])
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("tee: {e}")),
        };
        let parsed = match TeeArgs::try_parse_from(
            std::iter::once("tee".to_string()).chain(argv),
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

        // Under trash, a truncating overwrite snapshots the prior content
        // before we write below (no-op with trash off). Append is excluded
        // from that snapshot because it carries the prior bytes forward
        // itself: the backend folds them into the new write, or reports the
        // error and writes nothing. That exclusion is only safe while a
        // failed read cannot stand in for empty content — which is why
        // append goes through the backend rather than reading here.
        // A writer racing between the backend's read and its write is a
        // separate, known residual (see the CAS note at the write site),
        // not something a snapshot would catch either.
        let targets: Vec<(String, bool)> = paths.iter().map(|p| (p.clone(), append)).collect();
        let snapshots = match ctx
            .snapshot_overwrites("tee",
                &targets)
            .await
        {
            Ok(s) => s,
            Err(blocked) => return blocked,
        };

        // Read raw bytes so binary passes through tee intact (to files and to
        // the next stage).
        let input = match ctx.read_stdin_to_bytes().await {
            Ok(i) => i.unwrap_or_default(),
            Err(e) => return ExecResult::failure(1, format!("tee: {e}")),
        };

        // POSIX: tee writes stdin to every file AND to stdout. Continue past
        // per-file errors (matches POSIX `tee` semantics) and report every
        // failure so the agent sees the full picture, not just the last one.
        let mut errors: Vec<String> = Vec::new();
        for path_str in &paths {
            let resolved = ctx.resolve_path(path_str);
            let path = Path::new(&resolved);

            // Overwrite writes the borrowed input directly (no clone). A
            // truncating overwrite of a gated target goes through a CAS against
            // the gate's snapshot, so a concurrent change between the gate and
            // this write is a loud conflict, not a silent clobber.
            //
            // Append delegates to the backend rather than reading and writing
            // here. `KernelBackend::append` is the same primitive the `>>`
            // redirect uses, and it already treats a failed read correctly:
            // `NotFound` starts from an empty base (appending to a new file
            // creates it), while a permission error or an I/O error
            // propagates. Reading here instead let a failed read stand in for
            // empty content, so `tee -a` overwrote the file with just the new
            // input and exited 0.
            let write_result = if append {
                ctx.backend
                    .append(path, &input)
                    .await
                    .map_err(|e| e.to_string())
            } else {
                let expected = snapshots.get(&resolved);
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
            result.err = ExecResult::terminate_diagnostic(errors.join("\n"));
            result = result.with_code(1);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::WriteMode;
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

    /// Wraps a real backend and forces the append path to fail for one
    /// path, so tee's handling of a failed append is testable
    /// deterministically. Both `read` and `append` fail, so the test states
    /// tee's contract — report the failure, leave the bytes alone — without
    /// pinning which backend primitive tee reaches for. (A real mode-0200
    /// file is the end-to-end fixture; see
    /// `tests/tee_append_read_failure_tests.rs`.)
    struct FailingReadBackend {
        inner: Arc<dyn crate::backend::KernelBackend>,
        fail_path: std::path::PathBuf,
    }

    #[async_trait::async_trait]
    impl crate::backend::KernelBackend for FailingReadBackend {
        async fn read(
            &self,
            path: &Path,
            range: Option<crate::backend::ReadRange>,
        ) -> crate::backend::BackendResult<Vec<u8>> {
            if path == self.fail_path {
                return Err(crate::backend::BackendError::PermissionDenied(
                    path.display().to_string(),
                ));
            }
            self.inner.read(path, range).await
        }
        async fn write(
            &self,
            path: &Path,
            content: &[u8],
            mode: WriteMode,
        ) -> crate::backend::BackendResult<()> {
            self.inner.write(path, content, mode).await
        }
        async fn append(&self, path: &Path, content: &[u8]) -> crate::backend::BackendResult<()> {
            if path == self.fail_path {
                return Err(crate::backend::BackendError::PermissionDenied(
                    path.display().to_string(),
                ));
            }
            self.inner.append(path, content).await
        }
        async fn patch(
            &self,
            path: &Path,
            ops: &[crate::backend::PatchOp],
        ) -> crate::backend::BackendResult<()> {
            self.inner.patch(path, ops).await
        }
        async fn list(&self, path: &Path) -> crate::backend::BackendResult<Vec<crate::vfs::DirEntry>> {
            self.inner.list(path).await
        }
        async fn stat(&self, path: &Path) -> crate::backend::BackendResult<crate::vfs::DirEntry> {
            self.inner.stat(path).await
        }
        async fn mkdir(&self, path: &Path) -> crate::backend::BackendResult<()> {
            self.inner.mkdir(path).await
        }
        async fn set_mtime(
            &self,
            path: &Path,
            mtime: std::time::SystemTime,
        ) -> crate::backend::BackendResult<()> {
            self.inner.set_mtime(path, mtime).await
        }
        async fn remove(&self, path: &Path, recursive: bool) -> crate::backend::BackendResult<()> {
            self.inner.remove(path, recursive).await
        }
        async fn rename(&self, from: &Path, to: &Path) -> crate::backend::BackendResult<()> {
            self.inner.rename(from, to).await
        }
        async fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path).await
        }
        async fn lstat(&self, path: &Path) -> crate::backend::BackendResult<crate::vfs::DirEntry> {
            self.inner.lstat(path).await
        }
        async fn read_link(&self, path: &Path) -> crate::backend::BackendResult<std::path::PathBuf> {
            self.inner.read_link(path).await
        }
        async fn symlink(&self, target: &Path, link: &Path) -> crate::backend::BackendResult<()> {
            self.inner.symlink(target, link).await
        }
        async fn call_tool(
            &self,
            name: &str,
            args: ToolArgs,
            ctx: &mut dyn ToolCtx,
        ) -> crate::backend::BackendResult<crate::backend::ToolResult> {
            self.inner.call_tool(name, args, ctx).await
        }
        async fn list_tools(&self) -> crate::backend::BackendResult<Vec<crate::backend::ToolInfo>> {
            self.inner.list_tools().await
        }
        async fn get_tool(
            &self,
            name: &str,
        ) -> crate::backend::BackendResult<Option<crate::backend::ToolInfo>> {
            self.inner.get_tool(name).await
        }
        fn read_only(&self) -> bool {
            self.inner.read_only()
        }
        fn backend_type(&self) -> &str {
            self.inner.backend_type()
        }
        fn mounts(&self) -> Vec<crate::backend::MountInfo> {
            self.inner.mounts()
        }
        fn resolve_real_path(&self, path: &Path) -> Option<std::path::PathBuf> {
            self.inner.resolve_real_path(path)
        }
    }

    #[tokio::test]
    async fn test_tee_append_read_failure_does_not_truncate() {
        use crate::backend::KernelBackend as _;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("existing.txt"), b"original content\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        let vfs = Arc::new(vfs);

        // Two independent backends over the *same* VFS: `backend` is what
        // tee writes through (and always fails to read `/existing.txt`);
        // `verify_backend` is a plain, un-wrapped view used only to inspect
        // what actually landed on disk, so the assertion below isn't
        // laundered through the same backend that's rigged to fail.
        let inner: Arc<dyn crate::backend::KernelBackend> =
            Arc::new(crate::backend::LocalBackend::new(vfs.clone()));
        let verify_backend = crate::backend::LocalBackend::new(vfs);
        let backend: Arc<dyn crate::backend::KernelBackend> = Arc::new(FailingReadBackend {
            inner,
            fail_path: std::path::PathBuf::from("/existing.txt"),
        });
        let mut ctx = ExecContext::with_backend(backend);
        ctx.set_stdin("appended\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));
        args.flags.insert("a".to_string());

        let result = Tee.execute(args, &mut ctx).await;

        assert!(!result.ok(), "tee -a must fail when the pre-append read fails");
        assert!(
            !result.err.is_empty(),
            "the read failure must be reported, not swallowed"
        );

        // The heart of this test: the file must still hold its ORIGINAL
        // bytes, not just the new input alone.
        let written = verify_backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(
            written, b"original content\n",
            "a failed pre-append read must not truncate the file; got {written:?}"
        );
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
        let snapshot = crate::tools::OverwriteExpectation::Bytes(b"original content\n".to_vec());

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
        let snapshot = crate::tools::OverwriteExpectation::Bytes(b"original content\n".to_vec());

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
        let empty_snapshot = crate::tools::OverwriteExpectation::Bytes(Vec::new());
        ctx.backend.remove(path, false).await.unwrap();

        // A failed re-read must surface loudly — not be swallowed to `[]` and
        // false-match the empty snapshot, which would silently (re)write.
        let result = ctx
            .overwrite_checked(path, b"new\n", Some(&empty_snapshot))
            .await;
        assert!(result.is_err(), "vanished target must error, got {result:?}");
    }
}
