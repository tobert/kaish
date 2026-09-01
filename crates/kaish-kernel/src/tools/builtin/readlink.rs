//! readlink — Print symlink target or resolved path.
//!
//! Without flags, prints the raw symlink target (errors on non-symlinks).
//! With -f, canonicalizes the path by following all symlinks through the VFS.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use kaish_types::backend::MountInfo;

/// Maximum symlink hops to prevent infinite loops (matches Linux MAXSYMLINKS).
const MAX_SYMLINK_HOPS: usize = 40;

/// Readlink tool: read symlink target or canonicalize a path.
pub struct Readlink;

/// clap-derived argv layer for readlink.
#[derive(Parser, Debug)]
#[command(name = "readlink", about = "Print symlink target or resolved path")]
struct ReadlinkArgs {
    /// Resolve to canonical absolute path (-f)
    #[arg(short = 'f', long = "canonicalize")]
    canonicalize: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Symlinks to read.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Readlink {
    fn name(&self) -> &str {
        "readlink"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ReadlinkArgs::command(),
            "readlink",
            "Print symlink target or resolved path",
            [
                ("Read symlink target", "readlink link.txt"),
                ("Canonicalize path", "readlink -f ../some/./path"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("readlink: {e}")),
        };
        let parsed = match ReadlinkArgs::try_parse_from(
            std::iter::once("readlink".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("readlink: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "readlink: missing path argument");
        }

        let canonicalize = parsed.canonicalize;
        let mut output = String::new();
        let mut last_err: Option<String> = None;
        let mut exit_code = 0i64;

        for value in &args.positional {
            let path_str = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("readlink: {e}")),
            };
            let resolved = ctx.resolve_path(&path_str);

            if canonicalize {
                // GNU readlink -f: canonicalize through symlinks.
                // Missing final component is allowed (resolves parents only).
                match canonicalize_path_allow_missing_final(ctx, &resolved).await {
                    Ok(canonical) => {
                        output.push_str(&canonical.to_string_lossy());
                        output.push('\n');
                    }
                    Err(msg) => {
                        last_err = Some(format!("readlink: {}: {}", path_str, msg));
                        exit_code = 1;
                    }
                }
                continue;
            }

            // Bare readlink: read the raw symlink target.
            // First check if the path is actually a symlink via lstat; if not,
            // emit the correct "not a symbolic link" message (not the OS EINVAL).
            match ctx.backend.lstat(Path::new(&resolved)).await {
                Ok(entry) => {
                    if !entry.is_symlink() {
                        last_err = Some(format!(
                            "readlink: {}: not a symbolic link",
                            path_str
                        ));
                        exit_code = 1;
                        continue;
                    }
                }
                Err(e) => {
                    use crate::backend::BackendError;
                    last_err = Some(match &e {
                        BackendError::NotFound(_) => {
                            format!("readlink: {}: No such file or directory", path_str)
                        }
                        _ => format!("readlink: {}: {}", path_str, e),
                    });
                    exit_code = 1;
                    continue;
                }
            }

            match ctx.backend.read_link(Path::new(&resolved)).await {
                Ok(target) => {
                    output.push_str(&target.display().to_string());
                    output.push('\n');
                }
                Err(e) => {
                    last_err = Some(format!("readlink: {}: {}", path_str, e));
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

/// Canonicalize a path through the VFS backend, following symlinks at every
/// component. Missing final component is allowed (GNU `readlink -f` semantics):
/// if the last component doesn't exist but its parent does, return the
/// normalized parent + final component.
///
/// A mount can be rooted below `/` (an embedder mounting `LocalFs` at a deep
/// VFS path, with another filesystem covering `/`). The components ABOVE that
/// mount point are structural — the router synthesizes them as directories —
/// not entries any single backend can `lstat`; querying one against whatever
/// backend happens to own `/` produced a bogus "No such file or directory"
/// naming the mount's first path component. `is_structural` recognizes those
/// components (via `KernelBackend::mounts`) and skips the `lstat`.
///
/// Returns an error if any intermediate (non-final) component is missing or
/// if symlink resolution loops.
pub async fn canonicalize_path_allow_missing_final(
    ctx: &ExecContext,
    path: &Path,
) -> Result<PathBuf, String> {
    let components: Vec<_> = path.components().collect();
    let total = components.len();

    if total == 0 {
        return Err("empty path".to_string());
    }

    let mounts = ctx.backend.mounts();
    let mut current = PathBuf::new();

    for (idx, component) in components.iter().enumerate() {
        let is_last = idx + 1 == total;

        match component {
            std::path::Component::RootDir => {
                current.push("/");
            }
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                current.pop();
            }
            std::path::Component::Normal(_) => {
                current.push(component);
                if !is_structural(&current, &mounts) {
                    // Resolve symlinks at this component.
                    current = resolve_symlink_component(ctx, &mounts, current, is_last).await?;
                }
            }
            std::path::Component::Prefix(_) => {
                current.push(component);
            }
        }
    }

    Ok(current)
}

/// True when `path` is a structural VFS path no single backend owns: a mount
/// point boundary itself, or a strict ancestor of one (`/tmp` above a mount
/// at `/tmp/x/fixture`). The router synthesizes both as directories rather
/// than asking a backend, so they can never be symlinks and never need an
/// `lstat` call.
///
/// A path genuinely covered by a real, more specific (non-`/`) mount is not
/// structural — that mount's own `lstat` answers for it, containment and all.
/// Longest-prefix match mirrors `VfsRouter::mount_of`'s own selection so this
/// agrees with how the router will actually route the path.
fn is_structural(path: &Path, mounts: &[MountInfo]) -> bool {
    match owning_mount(path, mounts) {
        // The path IS a mount point: the router synthesizes it, never
        // delegates to the mount's own root lstat.
        Some(m) if m == path => true,
        // A real, more specific mount covers this path — trust its lstat.
        Some(m) if m != Path::new("/") => false,
        // Only `/` (or nothing) covers this path directly. Structural only
        // if it sits strictly above some other mount's own root.
        _ => mounts.iter().any(|m| m.path != path && is_strict_prefix(path, &m.path)),
    }
}

/// The longest mount path that covers `path` — `path` equals it, or sits
/// beneath it — mirroring `VfsRouter::mount_of`'s own longest-prefix
/// selection. `None` when no mount covers `path` at all.
fn owning_mount(path: &Path, mounts: &[MountInfo]) -> Option<PathBuf> {
    mounts
        .iter()
        .map(|m| m.path.as_path())
        .filter(|m| *m == path || is_strict_prefix(m, path))
        .max_by_key(|m| m.as_os_str().len())
        .map(PathBuf::from)
}

/// True when `path` is a strict ancestor of `descendant` (`descendant`
/// starts with `path` plus a separator). Handles `path == "/"` without
/// producing a doubled slash.
fn is_strict_prefix(path: &Path, descendant: &Path) -> bool {
    let path_str = path.to_string_lossy();
    let descendant_str = descendant.to_string_lossy();
    if path_str == "/" {
        descendant_str.len() > 1 && descendant_str.starts_with('/')
    } else {
        descendant_str.starts_with(&format!("{}/", path_str))
    }
}

/// Resolve potential symlinks at `path`, following the chain up to
/// `MAX_SYMLINK_HOPS`. If `allow_missing` is true and the path doesn't exist,
/// return `path` unchanged (the caller already knows it's the final component).
async fn resolve_symlink_component(
    ctx: &ExecContext,
    mounts: &[MountInfo],
    path: PathBuf,
    allow_missing: bool,
) -> Result<PathBuf, String> {
    // The mount that owns the starting path. A rooted (non-`/`) mount is a
    // containment boundary: a symlink hop that lands the walk outside it —
    // an absolute target elsewhere on the host, or enough `..` to climb out
    // — must be refused, the same guarantee `resolve_beneath` gives a single
    // `LocalFs`. Root (`/`) draws no such boundary: it's the unrooted case
    // every existing symlink walks across freely.
    let start_owner = owning_mount(&path, mounts);
    let mut current = path;

    for _ in 0..MAX_SYMLINK_HOPS {
        // A symlink target can jump onto a structural path (e.g. an absolute
        // target landing above another mount); re-check every hop, not just
        // the outer walk's own components.
        if is_structural(&current, mounts) {
            return Ok(current);
        }
        match ctx.backend.lstat(Path::new(&current)).await {
            Ok(entry) if entry.is_symlink() => {
                let target = ctx
                    .backend
                    .read_link(Path::new(&current))
                    .await
                    .map_err(|e| e.to_string())?;

                if target.is_absolute() {
                    current = target;
                } else {
                    // Relative target: resolve from the link's parent directory.
                    let parent = current.parent().unwrap_or(Path::new("/"));
                    current = parent.join(target);
                }
                // Normalize out any . and .. introduced by the target.
                current = normalize_path_buf(current);

                if let Some(start) = &start_owner
                    && start.as_path() != Path::new("/")
                    && owning_mount(&current, mounts).as_deref() != Some(start.as_path())
                {
                    return Err(format!(
                        "path escapes root: {} is not under {}",
                        current.display(),
                        start.display()
                    ));
                }
            }
            Ok(_) => {
                // Not a symlink — resolved.
                return Ok(current);
            }
            Err(e) => {
                use crate::backend::BackendError;
                match &e {
                    BackendError::NotFound(_) if allow_missing => {
                        // Final component missing — allowed per GNU readlink -f.
                        return Ok(current);
                    }
                    BackendError::NotFound(_) => {
                        return Err(format!(
                            "No such file or directory: {}",
                            current.display()
                        ));
                    }
                    _ => return Err(e.to_string()),
                }
            }
        }
    }

    Err(format!(
        "too many levels of symbolic links: {}",
        current.display()
    ))
}

/// Normalize a PathBuf by collapsing `.` and `..` components without
/// filesystem access. This handles targets injected by symlink resolution.
fn normalize_path_buf(path: PathBuf) -> PathBuf {
    let mut components: Vec<std::ffi::OsString> = Vec::new();
    let is_absolute = path.is_absolute();

    for component in path.components() {
        match component {
            std::path::Component::RootDir => {}
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                // A `..` cancels a preceding *normal* component, but on a
                // relative path it must ACCUMULATE past the start (and past
                // other leading `..`s): `../../a` normalizes to `../../a`, not
                // `a`. Only pop when the last component is a real name.
                let last_is_parent = components
                    .last()
                    .map(|s| s.as_os_str() == std::ffi::OsStr::new(".."))
                    .unwrap_or(false);
                if components.is_empty() || last_is_parent {
                    if !is_absolute {
                        components.push("..".into());
                    }
                    // Absolute: a leading `..` at root is a no-op (stays at /).
                } else {
                    components.pop();
                }
            }
            std::path::Component::Normal(s) => {
                components.push(s.to_os_string());
            }
            std::path::Component::Prefix(_) => {}
        }
    }

    if is_absolute {
        let mut result = PathBuf::from("/");
        for c in components {
            result.push(c);
        }
        result
    } else if components.is_empty() {
        PathBuf::from(".")
    } else {
        components.iter().collect()
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
    async fn test_readlink_canonicalize_dotdot_only() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/some/../path/./file".into()));
        args.flags.insert("f".to_string());

        let result = Readlink.execute(args, &mut ctx).await;
        // /some/../path/./file normalizes to /path/file; MemoryFs will report
        // "not found" for /path/file, which readlink -f allows as missing final component.
        // The parent /path must also not exist — let's test that the path normalizes at
        // least through the dot-dot without crashing.
        // (The actual test for symlink traversal is in the integration tests.)
        let _ = result; // just confirm it doesn't panic
    }

    #[tokio::test]
    async fn test_readlink_without_flag_on_regular_file_fails() {
        use crate::vfs::Filesystem;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("regular.txt"), b"content").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/regular.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a symbolic link"), "got: {}", result.err);
    }

    #[tokio::test]
    async fn test_readlink_missing_path() {
        let mut ctx = make_ctx();
        let result = Readlink.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    async fn make_ctx_with_symlink() -> ExecContext {
        use crate::vfs::Filesystem;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        mem.write(Path::new("target.txt"), b"content").await.unwrap();
        mem.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();
        mem.symlink(Path::new("nonexistent"), Path::new("broken.txt")).await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_readlink_symlink_target() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/link.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "target.txt");
    }

    #[tokio::test]
    async fn test_readlink_on_regular_file() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/target.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a symbolic link"), "got: {}", result.err);
    }

    #[tokio::test]
    async fn test_readlink_broken_symlink() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/broken.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "nonexistent");
    }

    #[tokio::test]
    async fn test_readlink_not_found() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/does-not-exist".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("No such file"), "got: {}", result.err);
    }

    #[test]
    fn test_normalize_path_buf_absolute() {
        assert_eq!(
            normalize_path_buf(PathBuf::from("/usr/bin/../lib")),
            PathBuf::from("/usr/lib")
        );
        assert_eq!(
            normalize_path_buf(PathBuf::from("/usr/./bin")),
            PathBuf::from("/usr/bin")
        );
    }

    #[test]
    fn test_normalize_path_buf_relative() {
        assert_eq!(
            normalize_path_buf(PathBuf::from("a/b/../c")),
            PathBuf::from("a/c")
        );
    }

    #[test]
    fn test_normalize_path_buf_relative_leading_parents_accumulate() {
        // Regression (Gemini review): leading/consecutive `..` on a relative
        // path must accumulate, not cancel each other — `../../a` is `../../a`,
        // not `a`. A trailing `..` past the start likewise accumulates.
        assert_eq!(
            normalize_path_buf(PathBuf::from("../../a")),
            PathBuf::from("../../a")
        );
        assert_eq!(
            normalize_path_buf(PathBuf::from("../a/../..")),
            PathBuf::from("../..")
        );
        // A `..` still cancels a preceding real component.
        assert_eq!(
            normalize_path_buf(PathBuf::from("../a/b/..")),
            PathBuf::from("../a")
        );
    }
}
