//! stat — Display file status.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{exec_context, schema_from_clap, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Stat tool: display file or filesystem status.
pub struct Stat;

/// clap-derived argv layer for stat.
#[derive(Parser, Debug)]
#[command(name = "stat", about = "Display file status")]
struct StatArgs {
    /// Output format (--format). Supports: %n (name), %s (size), %F (type)
    #[arg(short = 'c', long)]
    format: Option<String>,

    /// Follow the final symlink and describe its target instead of the link itself.
    #[arg(short = 'L', long = "dereference")]
    dereference: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to stat.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Stat {
    fn name(&self) -> &str {
        "stat"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &StatArgs::command(),
            "stat",
            "Display file status",
            [
                ("Show file info", "stat README.md"),
                ("Just the size", "stat --format '%s' file.txt"),
                ("Follow a symlink to its target", "stat -L link.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("stat: {e}")),
        };
        let parsed = match StatArgs::try_parse_from(
            std::iter::once("stat".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("stat: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(2, "stat: missing path argument");
        }

        let format = args
            .get_string("format", usize::MAX)
            .or_else(|| args.get_string("c", usize::MAX));

        // GNU: `stat a b c` stats each. Format mode prints one line per
        // file; default mode collects rows into one table. Continue past
        // errors and report the last failure so users see what worked.
        let mut nodes: Vec<OutputNode> = Vec::with_capacity(args.positional.len());
        let mut format_output = String::new();
        // The default text prints times in the exported `TZ`, else host local.
        let zone = if format.is_none() {
            match super::date::scope_zone(ctx) {
                Ok(zone) => Some(zone),
                Err(e) => return ExecResult::failure(1, format!("stat: {e}")),
            }
        } else {
            None
        };
        let mut text = String::new();
        let mut last_err: Option<String> = None;

        for value in &args.positional {
            let path_str = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("stat: {e}")),
            };
            let resolved = ctx.resolve_path(&path_str);
            // Default describes the link itself (lstat); -L follows to the
            // target, matching GNU `stat` (which does not follow by default).
            let stat_result = if parsed.dereference {
                ctx.backend.stat(Path::new(&resolved)).await
            } else {
                ctx.backend.lstat(Path::new(&resolved)).await
            };
            match stat_result {
                Ok(info) => {
                    if let Some(fmt) = &format {
                        format_output.push_str(&format_stat(fmt, &path_str, &info));
                    } else {
                        text.push_str(&stat_block(&path_str, &info, zone.as_ref()));
                        let is_symlink = info.is_symlink();
                        let is_dir = info.is_dir();
                        let file_type = file_type_name(&info);
                        let entry_type = if is_symlink {
                            EntryType::Symlink
                        } else if is_dir {
                            EntryType::Directory
                        } else {
                            EntryType::File
                        };
                        let mut cells = vec![info.size.to_string(), file_type.to_string()];
                        // TARGET is a fourth header; only a symlink row adds
                        // this third cell, so a file/dir row's JSON object
                        // (headers zipped positionally against cells) never
                        // gets a "TARGET" key at all.
                        if is_symlink {
                            let target = info
                                .symlink_target
                                .as_ref()
                                .map(|t| t.display().to_string())
                                .unwrap_or_default();
                            cells.push(target);
                        }
                        nodes.push(
                            OutputNode::new(&path_str)
                                .with_cells(cells)
                                .with_entry_type(entry_type),
                        );
                    }
                }
                Err(e) => {
                    last_err = Some(format!("stat: {}: {}", path_str, e));
                }
            }
        }

        // Emit accumulated output even when some paths failed (matches GNU).
        let mut result = if format.is_some() {
            ExecResult::with_output(OutputData::text(format_output))
        } else {
            let headers = vec![
                "FILE".to_string(),
                "SIZE".to_string(),
                "TYPE".to_string(),
                "TARGET".to_string(),
            ];
            ExecResult::with_output_and_text(OutputData::table(headers, nodes), text)
        };
        if let Some(msg) = last_err {
            result.err = ExecResult::terminate_diagnostic(msg);
            result = result.with_code(1);
        }
        result
    }
}

/// GNU's name for the file type, as `%F` and the default output print it.
fn file_type_name(info: &crate::vfs::DirEntry) -> &'static str {
    use kaish_types::DirEntryKind;
    match info.kind {
        DirEntryKind::Directory => "directory",
        DirEntryKind::Symlink => "symbolic link",
        DirEntryKind::CharDevice => "character special file",
        DirEntryKind::BlockDevice => "block special file",
        DirEntryKind::Fifo => "fifo",
        DirEntryKind::Socket => "socket",
        _ if info.size == 0 => "regular empty file",
        _ => "regular file",
    }
}

/// GNU `stat`'s default block for one file. Lines and fields kaish has no
/// data for (device, inode, IO block, access, change, and birth times) are
/// left out; a field the backend does not track prints `?`.
fn stat_block(name: &str, info: &crate::vfs::DirEntry, zone: Option<&super::date::Zone>) -> String {
    let unknown = || "?".to_string();
    let file = match (&info.symlink_target, info.is_symlink()) {
        (Some(target), true) => format!("{name} -> {}", target.display()),
        _ => name.to_string(),
    };
    let blocks = info.blocks.map_or_else(unknown, |b| b.to_string());
    let links = info.links.map_or_else(unknown, |n| n.to_string());
    let access = match info.permissions {
        Some(mode) => format!("{:04o}/{}", mode & 0o7777, super::ls::mode_string(info)),
        None => format!("?/{}", super::ls::mode_string(info)),
    };
    let account = |a: &Option<kaish_types::AccountId>| match a {
        Some(a) => format!("{:>5}/{:>8}", a.id, a.display_name()),
        None => format!("{:>5}/{:>8}", "?", "?"),
    };
    let modify = match (info.modified, zone) {
        (Some(modified), Some(zone)) => zone
            .instant(chrono::DateTime::<chrono::Utc>::from(modified))
            .format("%Y-%m-%d %H:%M:%S%.9f %z")
            .to_string(),
        _ => unknown(),
    };
    format!(
        "  File: {file}\n  Size: {:<10}\tBlocks: {blocks:<10} {}\n Links: {links}\nAccess: ({access})  Uid: ({})   Gid: ({})\nModify: {modify}\n",
        info.size,
        file_type_name(info),
        account(&info.owner),
        account(&info.group),
    )
}

/// Format stat output according to a format string.
fn format_stat(fmt: &str, name: &str, info: &crate::vfs::DirEntry) -> String {
    let mut result = fmt.to_string();

    // %n - file name
    result = result.replace("%n", name);

    // %s - size in bytes
    result = result.replace("%s", &info.size.to_string());

    // %F - file type
    result = result.replace("%F", file_type_name(info));

    // Add newline if not present
    if !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::ExecContext;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("file.txt"), b"hello world")
            .await
            .unwrap();
        mem.mkdir(Path::new("mydir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_stat_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Check OutputData structure
        assert!(result.has_output());
        let output = result.output().unwrap();
        assert!(output.headers.is_some());
        assert_eq!(output.root.len(), 1);
        // Canonical output contains file info
        assert!(result.text_out().contains("file.txt"));
        assert!(result.text_out().contains("11")); // "hello world" = 11 bytes
    }

    #[tokio::test]
    async fn test_stat_directory() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/mydir".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.has_output());
        let output = result.output().unwrap();
        assert_eq!(output.root.len(), 1);
        // Check canonical output contains directory
        assert!(result.text_out().contains("directory"));
    }

    #[tokio::test]
    async fn test_stat_format() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named
            .insert("format".to_string(), Value::String("%n: %s bytes".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "/file.txt: 11 bytes");
    }

    #[tokio::test]
    async fn test_stat_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_stat_missing_path() {
        let mut ctx = make_ctx().await;
        let result = Stat.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
