//! ls — List directory contents.

use async_trait::async_trait;
use std::cmp::Ordering;
use std::path::Path;

use crate::ast::Value;
use crate::backend::EntryInfo;
use crate::interpreter::{EntryType, ExecResult};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::walker::IgnoreFilter;

/// Ls tool: list directory contents.
pub struct Ls;

#[async_trait]
impl Tool for Ls {
    fn name(&self) -> &str {
        "ls"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("ls", "List directory contents")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String(".".into()),
                "Directory path to list",
            ))
            .param(ParamSchema::optional(
                "long",
                "bool",
                Value::Bool(false),
                "Use long format with details (-l)",
            ).with_aliases(["-l"]))
            .param(ParamSchema::optional(
                "all",
                "bool",
                Value::Bool(false),
                "Show hidden files starting with . (-a)",
            ).with_aliases(["-a", "-A"]))
            .param(ParamSchema::optional(
                "one",
                "bool",
                Value::Bool(false),
                "One entry per line (-1)",
            ).with_aliases(["-1"]))
            .param(ParamSchema::optional(
                "human",
                "bool",
                Value::Bool(false),
                "Human-readable sizes (-h)",
            ).with_aliases(["-h"]))
            .param(ParamSchema::optional(
                "sort_time",
                "bool",
                Value::Bool(false),
                "Sort by modification time (-t)",
            ).with_aliases(["-t"]))
            .param(ParamSchema::optional(
                "reverse",
                "bool",
                Value::Bool(false),
                "Reverse sort order (-r)",
            ).with_aliases(["-r"]))
            .param(ParamSchema::optional(
                "sort_size",
                "bool",
                Value::Bool(false),
                "Sort by file size (-S)",
            ).with_aliases(["-S"]))
            .param(ParamSchema::optional(
                "recursive",
                "bool",
                Value::Bool(false),
                "List subdirectories recursively (-R)",
            ).with_aliases(["-R"]))
            .example("List current directory", "ls")
            .example("Show hidden files with details", "ls -la /path")
            .example("Sort by size, largest first", "ls -lS /path")
            .example("Human-readable sizes", "ls -lh /path")
            .example("Recursive listing", "ls -R src/")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path = args
            .get_string("path", 0)
            .unwrap_or_else(|| ".".to_string());

        let resolved = ctx.resolve_path(&path);
        let long_format = args.has_flag("long") || args.has_flag("l");
        let show_all = args.has_flag("all") || args.has_flag("a");
        let human_readable = args.has_flag("human") || args.has_flag("h");
        let sort_time = args.has_flag("sort_time") || args.has_flag("t");
        let sort_size = args.has_flag("sort_size") || args.has_flag("S");
        let reverse = args.has_flag("reverse") || args.has_flag("r");
        let recursive = args.has_flag("recursive") || args.has_flag("R");

        let sort_opts = SortOptions {
            by_time: sort_time,
            by_size: sort_size,
            reverse,
        };

        // Check if path is a file (not a directory)
        // Real ls behavior: `ls file.txt` just outputs the filename
        if let Ok(info) = ctx.backend.stat(Path::new(&resolved)).await {
            if !info.is_dir {
                // It's a file - just display it
                return self.list_file(ctx, &path, &info, long_format, human_readable);
            }
        }

        if recursive {
            // Recursive listing
            self.list_recursive(
                ctx,
                &resolved,
                show_all,
                long_format,
                human_readable,
                &sort_opts,
            )
            .await
        } else {
            // Single directory listing
            self.list_single(
                ctx,
                &path,
                &resolved,
                show_all,
                long_format,
                human_readable,
                &sort_opts,
            )
            .await
        }
    }
}

struct SortOptions {
    by_time: bool,
    by_size: bool,
    reverse: bool,
}

impl Ls {
    /// List a single file (not a directory).
    /// Real `ls file.txt` just outputs the filename.
    fn list_file(
        &self,
        _ctx: &mut ExecContext,
        path: &str,
        info: &crate::backend::EntryInfo,
        long_format: bool,
        human_readable: bool,
    ) -> ExecResult {
        let output = if long_format {
            let type_char = '-'; // It's a file
            let size_str = if human_readable {
                format_human_size(info.size)
            } else {
                format!("{:>8}", info.size)
            };
            format!("{}  {}  {}", type_char, size_str, path)
        } else {
            path.to_string()
        };
        ExecResult::success(output)
    }

    async fn list_single(
        &self,
        ctx: &mut ExecContext,
        path: &str,
        resolved: &Path,
        show_all: bool,
        long_format: bool,
        human_readable: bool,
        sort_opts: &SortOptions,
    ) -> ExecResult {
        match ctx.backend.list(resolved).await {
            Ok(entries) => {
                let filtered = filter_and_sort(entries, show_all, sort_opts);

                if filtered.is_empty() {
                    return ExecResult::success("");
                }

                // Build table data and entry types for coloring
                let entry_types: Vec<EntryType> = filtered
                    .iter()
                    .map(entry_info_to_type)
                    .collect();

                let rows: Vec<Vec<String>> = if long_format {
                    // Name first (for canonical output), then type and size
                    filtered
                        .iter()
                        .map(|e| {
                            let type_char = if e.is_symlink {
                                "l"
                            } else if e.is_dir {
                                "d"
                            } else {
                                "-"
                            };
                            let size_str = if human_readable {
                                format_human_size(e.size)
                            } else {
                                format!("{:>8}", e.size)
                            };
                            // For symlinks, show "name -> target"
                            let name_display = if e.is_symlink {
                                if let Some(target) = &e.symlink_target {
                                    format!("{} -> {}", e.name, target.display())
                                } else {
                                    format!("{}@", e.name)
                                }
                            } else {
                                e.name.clone()
                            };
                            vec![name_display, type_char.to_string(), size_str]
                        })
                        .collect()
                } else {
                    // Single column - just names (with @ suffix for symlinks)
                    filtered
                        .iter()
                        .map(|e| {
                            let name_display = if e.is_symlink {
                                format!("{}@", e.name)
                            } else {
                                e.name.clone()
                            };
                            vec![name_display]
                        })
                        .collect()
                };

                if long_format {
                    ExecResult::success_table_with_headers(
                        vec!["Name".to_string(), "Type".to_string(), "Size".to_string()],
                        rows,
                        Some(entry_types),
                    )
                } else {
                    ExecResult::success_table(rows, Some(entry_types))
                }
            }
            Err(e) => ExecResult::failure(1, format!("ls: {}: {}", path, e)),
        }
    }

    async fn list_recursive(
        &self,
        ctx: &mut ExecContext,
        root: &Path,
        show_all: bool,
        long_format: bool,
        human_readable: bool,
        sort_opts: &SortOptions,
    ) -> ExecResult {
        let mut output = String::new();
        let mut dirs_to_visit: Vec<(String, String)> = vec![(
            root.to_string_lossy().to_string(),
            ".".to_string(),
        )];

        let ignore_filter = IgnoreFilter::with_defaults();

        while let Some((dir_path, display_path)) = dirs_to_visit.pop() {
            // List this directory
            let entries = match ctx.backend.list(Path::new(&dir_path)).await {
                Ok(e) => e,
                Err(_) => continue,
            };

            // Add header for this directory
            if !output.is_empty() {
                output.push('\n');
            }
            output.push_str(&display_path);
            output.push_str(":\n");

            let mut filtered = filter_and_sort(entries, show_all, sort_opts);

            // Filter out ignored directories
            filtered.retain(|e| {
                if e.is_dir && !show_all {
                    !ignore_filter.is_name_ignored(&e.name, true)
                } else {
                    true
                }
            });

            // Collect subdirs for recursion (before formatting)
            let subdirs: Vec<_> = filtered
                .iter()
                .filter(|e| e.is_dir)
                .map(|e| {
                    let child_path = format!("{}/{}", dir_path.trim_end_matches('/'), e.name);
                    let child_display = if display_path == "." {
                        e.name.clone()
                    } else {
                        format!("{}/{}", display_path, e.name)
                    };
                    (child_path, child_display)
                })
                .collect();

            // Format and output entries
            let lines = format_entries(&filtered, long_format, human_readable);
            output.push_str(&lines.join("\n"));
            if !lines.is_empty() {
                output.push('\n');
            }

            // Add subdirs to visit (in reverse order for DFS)
            for subdir in subdirs.into_iter().rev() {
                dirs_to_visit.push(subdir);
            }
        }

        ExecResult::success(output.trim_end().to_string())
    }
}

/// Filter hidden files and sort entries.
fn filter_and_sort(entries: Vec<EntryInfo>, show_all: bool, sort_opts: &SortOptions) -> Vec<EntryInfo> {
    let mut filtered: Vec<_> = entries
        .into_iter()
        .filter(|e| show_all || !e.name.starts_with('.'))
        .collect();

    filtered.sort_by(|a, b| {
        let cmp = if sort_opts.by_time {
            compare_by_time(a, b)
        } else if sort_opts.by_size {
            compare_by_size(a, b)
        } else {
            a.name.cmp(&b.name)
        };
        if sort_opts.reverse {
            cmp.reverse()
        } else {
            cmp
        }
    });

    filtered
}

/// Convert EntryInfo to EntryType for coloring.
fn entry_info_to_type(entry: &EntryInfo) -> EntryType {
    if entry.is_symlink {
        EntryType::Symlink
    } else if entry.is_dir {
        EntryType::Directory
    } else if entry.permissions.map(|p| p & 0o111 != 0).unwrap_or(false) {
        // Has execute permission
        EntryType::Executable
    } else {
        EntryType::File
    }
}

/// Format entries for output (used by recursive listing).
fn format_entries(entries: &[EntryInfo], long_format: bool, human_readable: bool) -> Vec<String> {
    if long_format {
        entries
            .iter()
            .map(|e| {
                let type_char = if e.is_symlink {
                    'l'
                } else if e.is_dir {
                    'd'
                } else {
                    '-'
                };
                let size_str = if human_readable {
                    format_human_size(e.size)
                } else {
                    format!("{:>8}", e.size)
                };
                // For symlinks, show the target
                let name_display = if e.is_symlink {
                    if let Some(target) = &e.symlink_target {
                        format!("{} -> {}", e.name, target.display())
                    } else {
                        format!("{}@", e.name)
                    }
                } else {
                    e.name.clone()
                };
                format!("{}  {}  {}", type_char, size_str, name_display)
            })
            .collect()
    } else {
        // In non-long format, add @ suffix for symlinks (like ls -F)
        entries.iter().map(|e| {
            if e.is_symlink {
                format!("{}@", e.name)
            } else {
                e.name.clone()
            }
        }).collect()
    }
}

/// Compare entries by modification time (newest first).
fn compare_by_time(a: &EntryInfo, b: &EntryInfo) -> Ordering {
    match (a.modified, b.modified) {
        (Some(ta), Some(tb)) => tb.cmp(&ta), // Newest first
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => a.name.cmp(&b.name),
    }
}

/// Compare entries by size (largest first).
fn compare_by_size(a: &EntryInfo, b: &EntryInfo) -> Ordering {
    b.size.cmp(&a.size) // Largest first
}

/// Format a size in human-readable form (1K, 2M, etc.).
fn format_human_size(bytes: u64) -> String {
    const UNITS: &[&str] = &["", "K", "M", "G", "T", "P"];
    let mut size = bytes as f64;
    let mut unit_idx = 0;

    while size >= 1024.0 && unit_idx < UNITS.len() - 1 {
        size /= 1024.0;
        unit_idx += 1;
    }

    if unit_idx == 0 {
        format!("{:>4}", bytes)
    } else if size >= 10.0 {
        format!("{:>3.0}{}", size, UNITS[unit_idx])
    } else {
        format!("{:>3.1}{}", size, UNITS[unit_idx])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("file1.txt"), b"a").await.unwrap();
        mem.write(Path::new("file2.txt"), b"b").await.unwrap();
        mem.mkdir(Path::new("subdir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_root() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("file1.txt"));
        assert!(result.out.contains("file2.txt"));
        assert!(result.out.contains("subdir"));
    }

    #[tokio::test]
    async fn test_ls_long() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("long".to_string(), Value::Bool(true));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Canonical output contains names (first column)
        assert!(result.out.contains("subdir"));
        assert!(result.out.contains("file1.txt"));
        // Check the hint has table data
        use crate::interpreter::DisplayHint;
        match &result.hint {
            DisplayHint::Table { rows, headers, .. } => {
                assert!(headers.is_some());
                assert!(!rows.is_empty());
                // Rows have name, type, size columns
                assert!(rows.iter().any(|r| r.len() == 3));
            }
            _ => panic!("Expected Table hint for long format"),
        }
    }

    #[tokio::test]
    async fn test_ls_cwd() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // cwd is /, should list root
        assert!(result.out.contains("file1.txt"));
    }

    #[tokio::test]
    async fn test_ls_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    async fn make_ctx_with_hidden() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("visible.txt"), b"a").await.unwrap();
        mem.write(Path::new(".hidden"), b"b").await.unwrap();
        mem.mkdir(Path::new(".config")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_hides_dotfiles_by_default() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("visible.txt"));
        assert!(!result.out.contains(".hidden"));
        assert!(!result.out.contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_a_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("a".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("visible.txt"));
        assert!(result.out.contains(".hidden"));
        assert!(result.out.contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_all_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("all".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains(".hidden"));
    }

    async fn make_ctx_with_subdirs() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();
        mem.write(Path::new("src/main.rs"), b"main").await.unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"utils")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"readme").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_recursive() {
        let mut ctx = make_ctx_with_subdirs().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("R".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should have directory headers
        assert!(result.out.contains(".:"));
        // Should show files in root
        assert!(result.out.contains("README.md"));
        assert!(result.out.contains("src"));
        // Should show files in src
        assert!(result.out.contains("main.rs"));
        // Should show nested directories
        assert!(result.out.contains("lib"));
    }

    #[test]
    fn test_entry_info_to_type_directory() {
        let entry = EntryInfo::directory("mydir");
        assert_eq!(entry_info_to_type(&entry), EntryType::Directory);
    }

    #[test]
    fn test_entry_info_to_type_file() {
        let entry = EntryInfo::file("test.txt", 100);
        assert_eq!(entry_info_to_type(&entry), EntryType::File);
    }

    #[test]
    fn test_entry_info_to_type_executable() {
        let mut entry = EntryInfo::file("script.sh", 100);
        entry.permissions = Some(0o755); // rwxr-xr-x
        assert_eq!(entry_info_to_type(&entry), EntryType::Executable);
    }

    #[test]
    fn test_entry_info_to_type_file_no_execute() {
        let mut entry = EntryInfo::file("data.txt", 100);
        entry.permissions = Some(0o644); // rw-r--r--
        assert_eq!(entry_info_to_type(&entry), EntryType::File);
    }

    #[test]
    fn test_entry_info_to_type_symlink() {
        let mut entry = EntryInfo::file("link.txt", 0);
        entry.is_symlink = true;
        entry.symlink_target = Some(std::path::PathBuf::from("target.txt"));
        assert_eq!(entry_info_to_type(&entry), EntryType::Symlink);
    }

    async fn make_ctx_with_symlinks() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("target.txt"), b"content").await.unwrap();
        mem.mkdir(Path::new("targetdir")).await.unwrap();
        mem.symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();
        mem.symlink(Path::new("targetdir"), Path::new("linkdir"))
            .await
            .unwrap();
        mem.symlink(Path::new("nonexistent"), Path::new("broken"))
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_shows_symlinks() {
        let mut ctx = make_ctx_with_symlinks().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show symlinks with @ suffix in short format
        assert!(result.out.contains("link.txt@"), "output was: {}", result.out);
        assert!(result.out.contains("linkdir@"), "output was: {}", result.out);
        assert!(result.out.contains("broken@"), "output was: {}", result.out);
    }

    #[tokio::test]
    async fn test_ls_long_shows_symlink_targets() {
        let mut ctx = make_ctx_with_symlinks().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("long".to_string(), Value::Bool(true));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Long format shows "name -> target"
        assert!(result.out.contains("link.txt -> target.txt"));
        assert!(result.out.contains("linkdir -> targetdir"));
        assert!(result.out.contains("broken -> nonexistent"));
        // Should have 'l' type character in table hint
        use crate::interpreter::DisplayHint;
        match &result.hint {
            DisplayHint::Table { rows, .. } => {
                // Find a row that has 'l' as the type
                let has_symlink_type = rows.iter().any(|r| r.get(1) == Some(&"l".to_string()));
                assert!(has_symlink_type, "Expected 'l' type character for symlinks");
            }
            _ => panic!("Expected Table hint for long format"),
        }
    }

    #[tokio::test]
    async fn test_ls_symlink_in_recursive() {
        let mut ctx = make_ctx_with_symlinks().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("R".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Symlinks should appear with @ suffix
        assert!(result.out.contains("link.txt@"));
    }
}
