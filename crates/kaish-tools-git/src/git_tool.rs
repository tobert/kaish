//! git — Version control operations via git2-rs.
//!
//! # Examples
//!
//! ```kaish
//! git init                        # Initialize a new repository
//! git clone https://url.git dir   # Clone a repository
//! git status                      # Show working tree status
//! git add src/*.rs                # Stage files
//! git commit -m "message"         # Create a commit
//! git log -n 5                    # Show recent commits
//! git diff                        # Show changes
//! git branch                      # List branches
//! git branch -c feature           # Create a new branch
//! git checkout main               # Switch branches
//! git worktree list               # List worktrees
//! git worktree add ../wt feature  # Create worktree for branch
//! git worktree remove wt-name     # Remove a worktree
//! git worktree prune              # Clean stale worktrees
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use kaish_types::Value;
use kaish_types::{ExecResult, OutputData};
use kaish_tool_api::{schema_from_clap, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};
use crate::git_vfs::GitVfs;

/// Git tool: version control operations via git2-rs.
pub struct Git;

/// clap-derived argv layer for git. See docs/clap-migration.md.
///
/// git multiplexes subcommands; rather than enumerate every subflag, the
/// struct declares the flags / named values our handlers consult (-s, -m,
/// -c, -b, -n, -f, --oneline, --short, --porcelain, --author, --reason)
/// plus a trailing positional sink. Subcommand-specific argv is read off
/// `args.positional` like before.
#[derive(Parser, Debug)]
#[command(name = "git", about = "Version control operations")]
struct GitArgs {
    /// Short status format (-s).
    #[arg(short = 's', long = "short")]
    short: bool,

    /// Porcelain status format.
    #[arg(long = "porcelain")]
    porcelain: bool,

    /// One-line log format.
    #[arg(long = "oneline")]
    oneline: bool,

    /// Force flag (-f) for worktree remove etc.
    #[arg(short = 'f', long = "force")]
    force: bool,

    /// Commit message.
    #[arg(short = 'm', long = "message")]
    message: Option<String>,

    /// Number of entries / count.
    #[arg(short = 'n', long = "count")]
    count: Option<String>,

    /// Branch name to create (-c).
    #[arg(short = 'c')]
    c: Option<String>,

    /// Branch name to create and checkout (-b).
    #[arg(short = 'b')]
    b: Option<String>,

    /// Author for commit.
    #[arg(long = "author")]
    author: Option<String>,

    /// Reason for worktree lock.
    #[arg(long = "reason")]
    reason: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Subcommand (`status`, `commit`, `branch`, …) followed by its arguments.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    subcommand: Vec<String>,
}

#[async_trait]
impl Tool for Git {
    fn name(&self) -> &str {
        "git"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &GitArgs::command(),
            "git",
            "Version control operations",
            [
                ("Show status", "git status"),
                ("Stage and commit", "git add file.rs; git commit -m 'fix bug'"),
                ("Recent log", "git log -n 5 --oneline"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        args.flagify_bool_named();

        let parsed = match GitArgs::try_parse_from(
            std::iter::once("git".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("git: {e}")),
        };
        parsed.global.apply(ctx);

        // Get subcommand
        let subcommand = match args.get_string("subcommand", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "git: missing subcommand"),
        };

        // Collect remaining positional args
        let rest_args: Vec<String> = args
            .positional
            .iter()
            .skip(1)
            .filter_map(|v| match v {
                Value::String(s) => Some(s.clone()),
                Value::Int(i) => Some(i.to_string()),
                _ => None,
            })
            .collect();

        // Route to subcommand handler
        match subcommand.as_str() {
            "init" => git_init(&rest_args, ctx).await,
            "clone" => git_clone(&rest_args, ctx).await,
            "status" => git_status(&args, ctx).await,
            "add" => git_add(&rest_args, ctx).await,
            "commit" => git_commit(&args, ctx).await,
            "log" => git_log(&args, ctx).await,
            "diff" => git_diff(ctx).await,
            "branch" => git_branch(&args, ctx).await,
            "checkout" => git_checkout(&rest_args, ctx).await,
            "worktree" => git_worktree(&args, &rest_args, ctx).await,
            _ => ExecResult::failure(1, format!("git: unknown subcommand '{}'", subcommand)),
        }
    }
}

/// Initialize a new git repository.
async fn git_init(args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    let vfs_path = if args.is_empty() {
        ctx.cwd().to_path_buf()
    } else {
        ctx.resolve_path(&args[0])
    };

    // Resolve VFS path to real filesystem path
    let real_path = match ctx.backend().resolve_real_path(&vfs_path) {
        Some(p) => p,
        None => {
            return ExecResult::failure(
                1,
                format!("git init: {} is not on a real filesystem", vfs_path.display()),
            )
        }
    };

    match GitVfs::init(&real_path) {
        Ok(_) => ExecResult::with_output(OutputData::text(format!(
            "Initialized empty Git repository in {}",
            vfs_path.display()
        ))),
        Err(e) => ExecResult::failure(1, format!("git init: {}", e)),
    }
}

/// Clone a repository.
async fn git_clone(args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git clone: missing repository URL");
    }

    let url = &args[0];
    let vfs_dest = if args.len() > 1 {
        ctx.resolve_path(&args[1])
    } else {
        // Extract repo name from URL
        let name = url
            .rsplit('/')
            .next()
            .unwrap_or("repo")
            .strip_suffix(".git")
            .unwrap_or(url.rsplit('/').next().unwrap_or("repo"));
        ctx.resolve_path(name)
    };

    // Resolve VFS path to real filesystem path
    // For clone, the destination doesn't exist yet, so resolve the parent
    let parent = vfs_dest.parent().unwrap_or(&vfs_dest);
    let real_parent = match ctx.backend().resolve_real_path(parent) {
        Some(p) => p,
        None => {
            return ExecResult::failure(
                1,
                format!("git clone: {} is not on a real filesystem", parent.display()),
            )
        }
    };
    let real_dest = if let Some(name) = vfs_dest.file_name() {
        real_parent.join(name)
    } else {
        real_parent
    };

    match GitVfs::clone(url, &real_dest) {
        Ok(_) => ExecResult::with_output(OutputData::text(format!("Cloning into '{}'...\ndone.", vfs_dest.display()))),
        Err(e) => ExecResult::failure(1, format!("git clone: {}", e)),
    }
}

/// Show repository status.
async fn git_status(args: &ToolArgs, ctx: &dyn ToolCtx) -> ExecResult {
    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    let short = args.has_flag("s") || args.has_flag("short");
    let porcelain = args.has_flag("porcelain");

    match git.status() {
        Ok(statuses) => {
            if statuses.is_empty() {
                if porcelain {
                    return ExecResult::success("");
                }
                return ExecResult::with_output(OutputData::text("nothing to commit, working tree clean"));
            }

            let mut output = String::new();

            if porcelain || short {
                // Short format: XY filename
                for file in &statuses {
                    output.push_str(file.status_char());
                    output.push(' ');
                    output.push_str(&file.path);
                    output.push('\n');
                }
            } else {
                // Long format
                let mut staged = Vec::new();
                let mut modified = Vec::new();
                let mut untracked = Vec::new();

                for file in &statuses {
                    if file.status.is_index_new()
                        || file.status.is_index_modified()
                        || file.status.is_index_deleted()
                    {
                        staged.push(&file.path);
                    }
                    if file.status.is_wt_modified() || file.status.is_wt_deleted() {
                        modified.push(&file.path);
                    }
                    if file.status.is_wt_new() && !file.status.is_index_new() {
                        untracked.push(&file.path);
                    }
                }

                if let Ok(Some(branch)) = git.current_branch() {
                    output.push_str(&format!("On branch {}\n\n", branch));
                }

                if !staged.is_empty() {
                    output.push_str("Changes to be committed:\n");
                    for path in staged {
                        output.push_str(&format!("  {}\n", path));
                    }
                    output.push('\n');
                }

                if !modified.is_empty() {
                    output.push_str("Changes not staged for commit:\n");
                    for path in modified {
                        output.push_str(&format!("  modified: {}\n", path));
                    }
                    output.push('\n');
                }

                if !untracked.is_empty() {
                    output.push_str("Untracked files:\n");
                    for path in untracked {
                        output.push_str(&format!("  {}\n", path));
                    }
                }
            }

            ExecResult::with_output(OutputData::text(output.trim_end()))
        }
        Err(e) => ExecResult::failure(1, format!("git status: {}", e)),
    }
}

/// Add files to the staging area.
async fn git_add(args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git add: missing pathspec");
    }

    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    // Convert args to references
    let pathspecs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();

    match git.add(&pathspecs) {
        Ok(()) => ExecResult::success(""),
        Err(e) => ExecResult::failure(1, format!("git add: {}", e)),
    }
}

/// Create a commit.
async fn git_commit(args: &ToolArgs, ctx: &dyn ToolCtx) -> ExecResult {
    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    // Get commit message
    let message = args
        .get_string("m", usize::MAX)
        .or_else(|| args.get_string("message", usize::MAX));

    let message = match message {
        Some(m) => m,
        None => return ExecResult::failure(1, "git commit: missing commit message (-m)"),
    };

    // Get author if specified
    let author = args.get_string("author", usize::MAX);

    match git.commit(&message, author.as_deref()) {
        Ok(oid) => {
            let short = &oid.to_string()[..7];
            ExecResult::with_output(OutputData::text(format!("[{short}] {message}")))
        }
        Err(e) => ExecResult::failure(1, format!("git commit: {}", e)),
    }
}

/// Show commit log.
async fn git_log(args: &ToolArgs, ctx: &dyn ToolCtx) -> ExecResult {
    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    // Get count (-n / count param)
    let count = args
        .get("count", usize::MAX)
        .and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        })
        .unwrap_or(10);

    let oneline = args.has_flag("oneline");

    match git.log(count) {
        Ok(entries) => {
            if entries.is_empty() {
                return ExecResult::with_output(OutputData::text("No commits yet"));
            }

            let mut output = String::new();

            for entry in entries {
                if oneline {
                    output.push_str(&format!("{} {}\n", entry.short_id, first_line(&entry.message)));
                } else {
                    output.push_str(&format!("commit {}\n", entry.oid));
                    output.push_str(&format!("Author: {} <{}>\n", entry.author, entry.email));
                    output.push_str(&format!(
                        "Date:   {}\n",
                        format_timestamp(entry.time)
                    ));
                    output.push_str(&format!("\n    {}\n\n", entry.message.trim()));
                }
            }

            ExecResult::with_output(OutputData::text(output.trim_end()))
        }
        Err(e) => ExecResult::failure(1, format!("git log: {}", e)),
    }
}

/// Show diff.
async fn git_diff(ctx: &dyn ToolCtx) -> ExecResult {
    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    match git.diff() {
        Ok(diff) => ExecResult::with_output(OutputData::text(diff.trim_end())),
        Err(e) => ExecResult::failure(1, format!("git diff: {}", e)),
    }
}

/// Branch operations.
async fn git_branch(args: &ToolArgs, ctx: &dyn ToolCtx) -> ExecResult {
    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    // Check for -c (create) or -b (create and checkout)
    let create = args.get_string("c", usize::MAX).or_else(|| args.get_string("b", usize::MAX));

    if let Some(name) = create {
        // Create a new branch
        match git.create_branch(&name) {
            Ok(()) => {
                // If -b, also checkout
                if args.has_flag("b") {
                    match git.checkout(&name) {
                        Ok(()) => {
                            return ExecResult::with_output(OutputData::text(format!("Switched to a new branch '{}'", name)))
                        }
                        Err(e) => return ExecResult::failure(1, format!("git checkout: {}", e)),
                    }
                }
                ExecResult::with_output(OutputData::text(format!("Branch '{}' created", name)))
            }
            Err(e) => ExecResult::failure(1, format!("git branch: {}", e)),
        }
    } else {
        // List branches
        match git.branches() {
            Ok(branches) => {
                let current = git.current_branch().ok().flatten();
                let mut output = String::new();

                for branch in branches {
                    let marker = if current.as_ref() == Some(&branch) {
                        "* "
                    } else {
                        "  "
                    };
                    output.push_str(&format!("{}{}\n", marker, branch));
                }

                ExecResult::with_output(OutputData::text(output.trim_end()))
            }
            Err(e) => ExecResult::failure(1, format!("git branch: {}", e)),
        }
    }
}

/// Checkout a branch or commit.
async fn git_checkout(args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git checkout: missing branch or commit");
    }

    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    let target = &args[0];

    match git.checkout(target) {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("Switched to '{}'", target))),
        Err(e) => ExecResult::failure(1, format!("git checkout: {}", e)),
    }
}

/// Worktree operations.
async fn git_worktree(args: &ToolArgs, rest_args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    if rest_args.is_empty() {
        return ExecResult::failure(1, "git worktree: missing subcommand (list, add, remove, lock, unlock, prune)");
    }

    let git = match open_repo(ctx) {
        Ok(g) => g,
        Err(e) => return e,
    };

    let subcmd = &rest_args[0];
    let subargs = &rest_args[1..];

    match subcmd.as_str() {
        "list" => worktree_list(&git),
        "add" => worktree_add(&git, subargs, ctx),
        "remove" => worktree_remove(&git, subargs, args),
        "lock" => worktree_lock(&git, subargs, args),
        "unlock" => worktree_unlock(&git, subargs),
        "prune" => worktree_prune(&git),
        _ => ExecResult::failure(1, format!("git worktree: unknown subcommand '{}'", subcmd)),
    }
}

/// List all worktrees.
fn worktree_list(git: &GitVfs) -> ExecResult {
    match git.worktrees() {
        Ok(worktrees) => {
            let mut output = String::new();
            for wt in worktrees {
                // Format: path  commit  [branch]
                let name_display = wt.name.as_deref().unwrap_or("(main)");
                let head_display = wt.head.as_deref().unwrap_or("(detached)");
                let lock_marker = if wt.locked { " [locked]" } else { "" };

                output.push_str(&format!(
                    "{:<40} {:<12} [{}]{}\n",
                    wt.path.display(),
                    head_display,
                    name_display,
                    lock_marker
                ));
            }
            ExecResult::with_output(OutputData::text(output.trim_end()))
        }
        Err(e) => ExecResult::failure(1, format!("git worktree list: {}", e)),
    }
}

/// Add a new worktree.
fn worktree_add(git: &GitVfs, args: &[String], ctx: &dyn ToolCtx) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git worktree add: missing path");
    }

    let path_arg = &args[0];
    let branch = args.get(1).map(|s| s.as_str());

    // Resolve the path relative to cwd
    let vfs_path = ctx.resolve_path(path_arg);

    // Get real filesystem path
    let real_path = match ctx.backend().resolve_real_path(&vfs_path) {
        Some(p) => p,
        None => {
            // If VFS path doesn't resolve, try resolving the parent
            let parent = vfs_path.parent().unwrap_or(&vfs_path);
            match ctx.backend().resolve_real_path(parent) {
                Some(p) => {
                    if let Some(name) = vfs_path.file_name() {
                        p.join(name)
                    } else {
                        p
                    }
                }
                None => {
                    return ExecResult::failure(
                        1,
                        format!("git worktree add: {} is not on a real filesystem", vfs_path.display()),
                    )
                }
            }
        }
    };

    // Derive worktree name from path
    let name = real_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("worktree");

    match git.worktree_add(name, &real_path, branch) {
        Ok(info) => {
            let branch_msg = info.head.as_deref().unwrap_or(name);
            ExecResult::with_output(OutputData::text(format!(
                "Preparing worktree (new branch '{}')\nHEAD is now at {}",
                branch_msg,
                info.path.display()
            )))
        }
        Err(e) => ExecResult::failure(1, format!("git worktree add: {}", e)),
    }
}

/// Remove a worktree.
fn worktree_remove(git: &GitVfs, args: &[String], tool_args: &ToolArgs) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git worktree remove: missing worktree name");
    }

    let name = &args[0];
    let force = tool_args.has_flag("f") || tool_args.has_flag("force");

    match git.worktree_remove(name, force) {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("Removed worktree '{}'", name))),
        Err(e) => ExecResult::failure(1, format!("git worktree remove: {}", e)),
    }
}

/// Lock a worktree.
fn worktree_lock(git: &GitVfs, args: &[String], tool_args: &ToolArgs) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git worktree lock: missing worktree name");
    }

    let name = &args[0];
    let reason = tool_args.get_string("reason", usize::MAX);

    match git.worktree_lock(name, reason.as_deref()) {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("Locked worktree '{}'", name))),
        Err(e) => ExecResult::failure(1, format!("git worktree lock: {}", e)),
    }
}

/// Unlock a worktree.
fn worktree_unlock(git: &GitVfs, args: &[String]) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git worktree unlock: missing worktree name");
    }

    let name = &args[0];

    match git.worktree_unlock(name) {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("Unlocked worktree '{}'", name))),
        Err(e) => ExecResult::failure(1, format!("git worktree unlock: {}", e)),
    }
}

/// Prune stale worktrees.
fn worktree_prune(git: &GitVfs) -> ExecResult {
    match git.worktree_prune() {
        Ok(count) => {
            if count == 0 {
                ExecResult::with_output(OutputData::text("Nothing to prune"))
            } else {
                ExecResult::with_output(OutputData::text(format!("Pruned {} stale worktree(s)", count)))
            }
        }
        Err(e) => ExecResult::failure(1, format!("git worktree prune: {}", e)),
    }
}

/// Open the git repository in the current working directory.
#[allow(clippy::result_large_err)]
fn open_repo(ctx: &dyn ToolCtx) -> Result<GitVfs, ExecResult> {
    // Resolve VFS path to real filesystem path
    let real_path = ctx.backend().resolve_real_path(ctx.cwd()).ok_or_else(|| {
        ExecResult::failure(
            128,
            format!(
                "fatal: not a git repository: {} is not on a real filesystem",
                ctx.cwd().display()
            ),
        )
    })?;

    GitVfs::open(&real_path).map_err(|e| {
        ExecResult::failure(
            128,
            format!("fatal: not a git repository: {}", e),
        )
    })
}

/// Get the first line of a string.
fn first_line(s: &str) -> &str {
    s.lines().next().unwrap_or(s)
}

/// Format a Unix timestamp as a human-readable date.
fn format_timestamp(secs: i64) -> String {
    use chrono::{DateTime, Utc};
    let dt = DateTime::from_timestamp(secs, 0).unwrap_or(DateTime::<Utc>::UNIX_EPOCH);
    dt.format("%a %b %d %H:%M:%S %Y %z").to_string()
}
