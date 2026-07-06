//! ls — List directory contents.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::cmp::Ordering;
use std::path::Path;

use crate::ast::Value;
use crate::glob::contains_glob;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::vfs::DirEntry;

/// Ls tool: list directory contents.
pub struct Ls;

/// clap-derived argv layer for ls.
///
/// `disable_help_flag = true` is required because `-h` is taken by `--human`,
/// not by clap's auto-injected `--help`.
#[derive(Parser, Debug)]
#[command(name = "ls", about = "List directory contents", disable_help_flag = true)]
struct LsArgs {
    /// Use long format with details.
    #[arg(short = 'l', long = "long")]
    long: bool,

    /// Show hidden files starting with `.`.
    #[arg(short = 'a', long = "all")]
    all: bool,

    /// One entry per line.
    #[arg(short = '1', long = "one")]
    one: bool,

    /// Human-readable sizes.
    #[arg(short = 'h', long = "human")]
    human: bool,

    /// Sort by modification time.
    #[arg(short = 't', long = "sort-time", visible_alias = "sort_time")]
    sort_time: bool,

    /// Reverse sort order.
    #[arg(short = 'r', long = "reverse")]
    reverse: bool,

    /// Sort by file size.
    #[arg(short = 'S', long = "sort-size", visible_alias = "sort_size")]
    sort_size: bool,

    /// List subdirectories recursively.
    #[arg(short = 'R', long = "recursive")]
    recursive: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Paths to list; defaults to the current directory.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Ls {
    fn name(&self) -> &str {
        "ls"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &LsArgs::command(),
            "ls",
            "List directory contents",
            [
                ("List current directory", "ls"),
                ("Show hidden files with details", "ls -la /path"),
                ("Sort by size, largest first", "ls -lS /path"),
                ("Human-readable sizes", "ls -lh /path"),
                ("Recursive listing", "ls -R src/"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Tests poke args.named.insert("long", Value::Bool(true)); to_argv would
        // emit `--long=true` which clap rejects for bool fields. Promote bool
        // named entries to flag form before clap parsing.
        args.flagify_bool_named(&self.schema());

        // The kaish lexer's ShortFlag regex is `-[a-zA-Z][a-zA-Z0-9-]*`, so `-1`
        // is tokenized as Int(-1) and lands in positionals. Intercept it here and
        // convert to the `one` flag before clap sees the argv.
        let had_dash_one = args.positional.iter().any(|v| matches!(v, Value::Int(-1)));
        if had_dash_one {
            args.positional.retain(|v| !matches!(v, Value::Int(-1)));
            args.flags.insert("1".to_string());
        }

        let parsed = match LsArgs::try_parse_from(
            std::iter::once("ls".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("ls: {e}")),
        };
        parsed.global.apply(ctx);

        let long_format = parsed.long;
        let show_all = parsed.all;
        let human_readable = parsed.human;
        let sort_time = parsed.sort_time;
        let sort_size = parsed.sort_size;
        let reverse = parsed.reverse;
        let recursive = parsed.recursive;
        // `one_per_line` is parsed so that `-1` is recognized as a flag (not an
        // error). Canonical (piped) output is already one-per-line for nodes;
        // the interactive multi-column renderer will need to respect this flag
        // separately — tracked in docs/issues.md.
        let _one_per_line = parsed.one;

        let opts = ListOptions {
            long_format,
            human_readable,
            show_all,
            sort: SortOptions {
                by_time: sort_time,
                by_size: sort_size,
                reverse,
            },
        };

        // Collect every positional path. The kernel pre-expands bare globs
        // into one positional per match, so a multi-element list is the norm
        // for `ls *.rs` / `ls a b c`. The old `get_string("path", 0)` read
        // only the first and silently dropped the rest.
        let mut paths: Vec<String> =
            match crate::interpreter::values_to_text_sink_named(&args.positional, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("ls: {e}")),
            };
        if paths.is_empty() {
            paths.push(".".to_string());
        }

        match paths.as_slice() {
            // Single target keeps the rich behavior: glob expansion,
            // file-shown-as-name, directory contents, and recursion.
            [path] => self.list_one(ctx, path, &opts, recursive).await,
            // Multiple targets list one node per argument — directories are
            // shown by name, not expanded — matching the predictable
            // structured-output model. Any glob that reached here unexpanded
            // (e.g. globbing disabled) is still expanded.
            many => {
                let mut names: Vec<String> = Vec::new();
                for path in many {
                    if contains_glob(path) {
                        match ctx.expand_paths(&[Value::String(path.clone())]).await {
                            Ok(expanded) => names.extend(expanded),
                            Err(e) => return ExecResult::failure(1, format!("ls: {e}")),
                        }
                    } else {
                        names.push(path.clone());
                    }
                }
                // Explicit operands: an inaccessible one is reported, not dropped.
                self.render_names(ctx, names, &opts, true).await
            }
        }
    }
}

struct SortOptions {
    by_time: bool,
    by_size: bool,
    reverse: bool,
}

/// Formatting options for ls output.
struct ListOptions {
    long_format: bool,
    human_readable: bool,
    show_all: bool,
    sort: SortOptions,
}

impl Ls {
    /// List a single file (not a directory).
    /// Real `ls file.txt` just outputs the filename.
    fn list_file(
        &self,
        _ctx: &mut ExecContext,
        path: &str,
        info: &DirEntry,
        opts: &ListOptions,
    ) -> ExecResult {
        let entry_type = dir_entry_to_type(info);
        let node = if opts.long_format {
            let type_char = if info.is_symlink() { "l" } else { "-" };
            let size_str = if opts.human_readable {
                format_human_size(info.size)
            } else {
                info.size.to_string()
            };
            OutputNode::new(path)
                .with_cells(vec![type_char.to_string(), size_str])
                .with_entry_type(entry_type)
        } else {
            OutputNode::new(path).with_entry_type(entry_type)
        };

        let output = if opts.long_format {
            OutputData::table(
                vec!["NAME".to_string(), "TYPE".to_string(), "SIZE".to_string()],
                vec![node],
            )
        } else {
            OutputData::nodes(vec![node])
        };

        ExecResult::with_output(output)
    }

    /// List files matching a glob pattern.
    async fn list_glob(
        &self,
        ctx: &mut ExecContext,
        pattern: &str,
        opts: &ListOptions,
    ) -> ExecResult {
        let patterns = [Value::String(pattern.to_string())];
        let names = match ctx.expand_paths(&patterns).await {
            Ok(n) => n,
            Err(e) => return ExecResult::failure(1, format!("ls: {}", e)),
        };
        // Glob matches were just walked; a stat race is benign → skip, don't error.
        self.render_names(ctx, names, opts, false).await
    }

    /// Single-target listing: glob expansion, a file shown as its own name,
    /// directory contents, or a recursive walk.
    async fn list_one(
        &self,
        ctx: &mut ExecContext,
        path: &str,
        opts: &ListOptions,
        recursive: bool,
    ) -> ExecResult {
        if contains_glob(path) {
            return self.list_glob(ctx, path, opts).await;
        }
        let resolved = ctx.resolve_path(path);
        // Real `ls file.txt` just echoes the filename rather than erroring.
        if let Ok(info) = ctx.backend.stat(Path::new(&resolved)).await
            && !info.is_dir()
        {
            return self.list_file(ctx, path, &info, opts);
        }
        if recursive {
            self.list_recursive(ctx, &resolved, opts).await
        } else {
            self.list_single(ctx, path, &resolved, opts).await
        }
    }

    /// Stat each named path, sort, and render one node per name. Shared by
    /// glob expansion and the multi-argument path.
    ///
    /// `report_missing` splits the two callers: an explicit operand
    /// (`ls real.txt gone.txt`) that fails to stat is `ls: cannot access` on
    /// stderr and a nonzero exit, like POSIX ls — dropping it silently would
    /// hide a typo. Glob expansion passes `false`: a match removed between the
    /// walk and the stat is a benign race, not a user error.
    async fn render_names(
        &self,
        ctx: &mut ExecContext,
        names: Vec<String>,
        opts: &ListOptions,
        report_missing: bool,
    ) -> ExecResult {
        if names.is_empty() {
            return ExecResult::with_output(OutputData::new());
        }

        // Stat each match and build DirEntry list for sorting/formatting
        let mut entries: Vec<(String, DirEntry)> = Vec::new();
        let mut error_text = String::new();
        for name in &names {
            let abs = ctx.resolve_path(name);
            match ctx.backend.stat(Path::new(&abs)).await {
                Ok(info) => entries.push((name.clone(), info)),
                Err(e) => {
                    if report_missing {
                        error_text.push_str(&format!("ls: cannot access '{}': {}\n", name, e));
                    }
                    // else: file disappeared between walk and stat; skip it
                }
            }
        }

        // Sort using the same logic as regular ls
        let mut infos: Vec<DirEntry> = entries
            .into_iter()
            .map(|(name, mut info)| {
                info.name = name;
                info
            })
            .collect();

        infos.sort_by(|a, b| {
            let cmp = if opts.sort.by_time {
                compare_by_time(a, b)
            } else if opts.sort.by_size {
                compare_by_size(a, b)
            } else {
                a.name.cmp(&b.name)
            };
            if opts.sort.reverse {
                cmp.reverse()
            } else {
                cmp
            }
        });

        let nodes: Vec<OutputNode> = infos
            .iter()
            .map(|e| {
                let entry_type = dir_entry_to_type(e);
                if opts.long_format {
                    let type_char = if e.is_symlink() {
                        "l"
                    } else if e.is_dir() {
                        "d"
                    } else {
                        "-"
                    };
                    let size_str = if opts.human_readable {
                        format_human_size(e.size)
                    } else {
                        e.size.to_string()
                    };
                    OutputNode::new(&e.name)
                        .with_cells(vec![type_char.to_string(), size_str])
                        .with_entry_type(entry_type)
                } else {
                    OutputNode::new(&e.name).with_entry_type(entry_type)
                }
            })
            .collect();

        let output = if opts.long_format {
            OutputData::table(
                vec!["NAME".to_string(), "TYPE".to_string(), "SIZE".to_string()],
                nodes,
            )
        } else {
            OutputData::nodes(nodes)
        };

        let mut result = ExecResult::with_output(output);
        // An inaccessible explicit operand is a loud error: stderr message plus
        // a nonzero exit (matching the single-arg `list_single` path's exit 1),
        // even though the readable operands still list successfully.
        if !error_text.is_empty() {
            result.err.push_str(&error_text);
            result.code = 1;
        }
        result
    }

    async fn list_single(
        &self,
        ctx: &mut ExecContext,
        path: &str,
        resolved: &Path,
        opts: &ListOptions,
    ) -> ExecResult {
        match ctx.backend.list(resolved).await {
            Ok(entries) => {
                let filtered = filter_and_sort(entries, opts.show_all, &opts.sort);

                if filtered.is_empty() {
                    return ExecResult::with_output(OutputData::new());
                }

                // Build OutputNodes for each entry
                let nodes: Vec<OutputNode> = filtered
                    .iter()
                    .map(|e| {
                        let entry_type = dir_entry_to_type(e);
                        let name_display = if e.is_symlink() {
                            if opts.long_format {
                                if let Some(target) = &e.symlink_target {
                                    format!("{} -> {}", e.name, target.display())
                                } else {
                                    format!("{}@", e.name)
                                }
                            } else {
                                format!("{}@", e.name)
                            }
                        } else {
                            e.name.clone()
                        };

                        if opts.long_format {
                            let type_char = if e.is_symlink() {
                                "l"
                            } else if e.is_dir() {
                                "d"
                            } else {
                                "-"
                            };
                            let size_str = if opts.human_readable {
                                format_human_size(e.size)
                            } else {
                                e.size.to_string()
                            };
                            OutputNode::new(name_display)
                                .with_cells(vec![type_char.to_string(), size_str])
                                .with_entry_type(entry_type)
                        } else {
                            OutputNode::new(name_display)
                                .with_entry_type(entry_type)
                        }
                    })
                    .collect();

                let output = if opts.long_format {
                    OutputData::table(
                        vec!["NAME".to_string(), "TYPE".to_string(), "SIZE".to_string()],
                        nodes,
                    )
                } else {
                    OutputData::nodes(nodes)
                };

                ExecResult::with_output(output)
            }
            Err(e) => ExecResult::failure(1, format!("ls: {}: {}", path, e)),
        }
    }

    async fn list_recursive(
        &self,
        ctx: &mut ExecContext,
        root: &Path,
        opts: &ListOptions,
    ) -> ExecResult {
        let mut text_output = String::new();
        let mut dir_nodes: Vec<OutputNode> = Vec::new();
        let mut dirs_to_visit: Vec<(String, String)> = vec![(
            root.to_string_lossy().to_string(),
            ".".to_string(),
        )];

        let ignore_filter = ctx.build_ignore_filter(root).await;

        while let Some((dir_path, display_path)) = dirs_to_visit.pop() {
            // List this directory
            let entries = match ctx.backend.list(Path::new(&dir_path)).await {
                Ok(e) => e,
                Err(_) => continue,
            };

            // Add header for this directory (text output)
            if !text_output.is_empty() {
                text_output.push('\n');
            }
            text_output.push_str(&display_path);
            text_output.push_str(":\n");

            let mut filtered = filter_and_sort(entries, opts.show_all, &opts.sort);

            // Filter out ignored directories
            if let Some(ref filter) = ignore_filter {
                filtered.retain(|e| {
                    if e.is_dir() && !opts.show_all {
                        !filter.is_name_ignored(&e.name, true)
                    } else {
                        true
                    }
                });
            }

            // Collect subdirs for recursion (before formatting)
            let subdirs: Vec<_> = filtered
                .iter()
                .filter(|e| e.is_dir())
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

            // Build OutputNodes for this directory's entries
            let child_nodes: Vec<OutputNode> = filtered.iter().map(|e| {
                let entry_type = dir_entry_to_type(e);
                let name = e.name.clone();
                if opts.long_format {
                    let type_char = if e.is_symlink() { "l" } else if e.is_dir() { "d" } else { "-" };
                    let size_str = if opts.human_readable {
                        format_human_size(e.size)
                    } else {
                        e.size.to_string()
                    };
                    OutputNode::new(name)
                        .with_cells(vec![type_char.to_string(), size_str])
                        .with_entry_type(entry_type)
                } else {
                    OutputNode::new(name).with_entry_type(entry_type)
                }
            }).collect();

            dir_nodes.push(
                OutputNode::new(&display_path)
                    .with_entry_type(EntryType::Directory)
                    .with_children(child_nodes)
            );

            // Format and output entries (text)
            let lines = format_entries(&filtered, opts.long_format, opts.human_readable);
            text_output.push_str(&lines.join("\n"));
            if !lines.is_empty() {
                text_output.push('\n');
            }

            // Add subdirs to visit (in reverse order for DFS)
            for subdir in subdirs.into_iter().rev() {
                dirs_to_visit.push(subdir);
            }
        }

        let output = if opts.long_format {
            OutputData::table(
                vec!["NAME".to_string(), "TYPE".to_string(), "SIZE".to_string()],
                dir_nodes,
            )
        } else {
            OutputData::nodes(dir_nodes)
        };
        ExecResult::with_output_and_text(output, text_output.trim_end().to_string())
    }
}

/// Filter hidden files and sort entries.
fn filter_and_sort(entries: Vec<DirEntry>, show_all: bool, sort_opts: &SortOptions) -> Vec<DirEntry> {
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

/// Convert DirEntry to EntryType for coloring.
fn dir_entry_to_type(entry: &DirEntry) -> EntryType {
    if entry.is_symlink() {
        EntryType::Symlink
    } else if entry.is_dir() {
        EntryType::Directory
    } else if entry.permissions.map(|p| p & 0o111 != 0).unwrap_or(false) {
        EntryType::Executable
    } else {
        EntryType::File
    }
}

/// Format entries for output (used by recursive listing).
fn format_entries(entries: &[DirEntry], long_format: bool, human_readable: bool) -> Vec<String> {
    if long_format {
        entries
            .iter()
            .map(|e| {
                let type_char = if e.is_symlink() {
                    'l'
                } else if e.is_dir() {
                    'd'
                } else {
                    '-'
                };
                let size_str = if human_readable {
                    format_human_size(e.size)
                } else {
                    e.size.to_string()
                };
                // For symlinks, show the target
                let name_display = if e.is_symlink() {
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
            if e.is_symlink() {
                format!("{}@", e.name)
            } else {
                e.name.clone()
            }
        }).collect()
    }
}

/// Compare entries by modification time (newest first).
fn compare_by_time(a: &DirEntry, b: &DirEntry) -> Ordering {
    match (a.modified, b.modified) {
        (Some(ta), Some(tb)) => tb.cmp(&ta), // Newest first
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => a.name.cmp(&b.name),
    }
}

/// Compare entries by size (largest first).
fn compare_by_size(a: &DirEntry, b: &DirEntry) -> Ordering {
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
        bytes.to_string()
    } else if size >= 10.0 {
        format!("{:.0}{}", size, UNITS[unit_idx])
    } else {
        format!("{:.1}{}", size, UNITS[unit_idx])
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
        assert!(result.text_out().contains("file1.txt"));
        assert!(result.text_out().contains("file2.txt"));
        assert!(result.text_out().contains("subdir"));
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
        assert!(result.text_out().contains("subdir"));
        assert!(result.text_out().contains("file1.txt"));
        // Check the output has table data
        match result.output() {
            Some(output) => {
                assert!(output.headers.is_some());
                assert!(!output.root.is_empty());
                // Nodes have name + 2 cells (type and size)
                assert!(output.root.iter().any(|n| n.cells.len() == 2));
            }
            None => panic!("Expected OutputData for long format"),
        }
    }

    #[tokio::test]
    async fn test_ls_cwd() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // cwd is /, should list root
        assert!(result.text_out().contains("file1.txt"));
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
        assert!(result.text_out().contains("visible.txt"));
        assert!(!result.text_out().contains(".hidden"));
        assert!(!result.text_out().contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_a_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("a".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("visible.txt"));
        assert!(result.text_out().contains(".hidden"));
        assert!(result.text_out().contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_all_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("all".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains(".hidden"));
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
        assert!(result.text_out().contains(".:"));
        // Should show files in root
        assert!(result.text_out().contains("README.md"));
        assert!(result.text_out().contains("src"));
        // Should show files in src
        assert!(result.text_out().contains("main.rs"));
        // Should show nested directories
        assert!(result.text_out().contains("lib"));
    }

    #[test]
    fn test_dir_entry_to_type_directory() {
        let entry = DirEntry::directory("mydir");
        assert_eq!(dir_entry_to_type(&entry), EntryType::Directory);
    }

    #[test]
    fn test_dir_entry_to_type_file() {
        let entry = DirEntry::file("test.txt", 100);
        assert_eq!(dir_entry_to_type(&entry), EntryType::File);
    }

    #[test]
    fn test_dir_entry_to_type_executable() {
        let mut entry = DirEntry::file("script.sh", 100);
        entry.permissions = Some(0o755); // rwxr-xr-x
        assert_eq!(dir_entry_to_type(&entry), EntryType::Executable);
    }

    #[test]
    fn test_dir_entry_to_type_file_no_execute() {
        let mut entry = DirEntry::file("data.txt", 100);
        entry.permissions = Some(0o644); // rw-r--r--
        assert_eq!(dir_entry_to_type(&entry), EntryType::File);
    }

    #[test]
    fn test_dir_entry_to_type_symlink() {
        let entry = DirEntry::symlink("link.txt", "target.txt");
        assert_eq!(dir_entry_to_type(&entry), EntryType::Symlink);
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
        assert!(result.text_out().contains("link.txt@"), "output was: {}", result.text_out());
        assert!(result.text_out().contains("linkdir@"), "output was: {}", result.text_out());
        assert!(result.text_out().contains("broken@"), "output was: {}", result.text_out());
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
        assert!(result.text_out().contains("link.txt -> target.txt"));
        assert!(result.text_out().contains("linkdir -> targetdir"));
        assert!(result.text_out().contains("broken -> nonexistent"));
        // Should have 'l' type character in output nodes
        match result.output() {
            Some(output) => {
                // Find a node that has 'l' as the type in cells
                let has_symlink_type = output.root.iter().any(|n| n.cells.first() == Some(&"l".to_string()));
                assert!(has_symlink_type, "Expected 'l' type character for symlinks");
            }
            None => panic!("Expected OutputData for long format"),
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
        assert!(result.text_out().contains("link.txt@"));
    }

    #[tokio::test]
    async fn test_ls_glob_txt() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("file1.txt"));
        assert!(result.text_out().contains("file2.txt"));
        assert!(!result.text_out().contains("subdir"));
    }

    #[tokio::test]
    async fn test_ls_glob_scoped_subdir() {
        let mut ctx = make_ctx_with_subdirs().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("src/*.rs".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(!result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_ls_glob_long_format() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));
        args.named.insert("long".to_string(), Value::Bool(true));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("file1.txt"));
        assert!(result.has_output());
        let output = result.output().cloned().unwrap();
        assert!(output.headers.is_some());
    }

    #[tokio::test]
    async fn test_ls_glob_no_matches() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.nonexistent".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }
}
