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

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::vfs::GitVfs;

/// Git tool: version control operations via git2-rs.
pub struct Git;

#[async_trait]
impl Tool for Git {
    fn name(&self) -> &str {
        "git"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("git", "Version control operations")
            .param(ParamSchema::required(
                "subcommand",
                "string",
                "Git subcommand (init, clone, status, add, commit, log, diff, branch, checkout, worktree)",
            ))
            .param(ParamSchema::optional(
                "args",
                "array",
                Value::Null,
                "Additional arguments for the subcommand",
            ))
            .param(ParamSchema::optional(
                "count",
                "int",
                Value::Null,
                "Number of entries to show (-n)",
            ).with_aliases(["-n"]))
            .example("Show status", "git status")
            .example("Stage and commit", "git add file.rs; git commit -m 'fix bug'")
            .example("Recent log", "git log -n 5 --oneline")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
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
async fn git_init(args: &[String], ctx: &ExecContext) -> ExecResult {
    let vfs_path = if args.is_empty() {
        ctx.cwd.clone()
    } else {
        ctx.resolve_path(&args[0])
    };

    // Resolve VFS path to real filesystem path
    let real_path = match ctx.backend.resolve_real_path(&vfs_path) {
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
async fn git_clone(args: &[String], ctx: &ExecContext) -> ExecResult {
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
    let real_parent = match ctx.backend.resolve_real_path(parent) {
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
async fn git_status(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
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
                    return ExecResult::with_output(OutputData::text(""));
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
async fn git_add(args: &[String], ctx: &ExecContext) -> ExecResult {
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
        Ok(()) => ExecResult::with_output(OutputData::text("")),
        Err(e) => ExecResult::failure(1, format!("git add: {}", e)),
    }
}

/// Create a commit.
async fn git_commit(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
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
async fn git_log(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
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
async fn git_diff(ctx: &ExecContext) -> ExecResult {
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
async fn git_branch(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
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
async fn git_checkout(args: &[String], ctx: &ExecContext) -> ExecResult {
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
async fn git_worktree(args: &ToolArgs, rest_args: &[String], ctx: &ExecContext) -> ExecResult {
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
fn worktree_add(git: &GitVfs, args: &[String], ctx: &ExecContext) -> ExecResult {
    if args.is_empty() {
        return ExecResult::failure(1, "git worktree add: missing path");
    }

    let path_arg = &args[0];
    let branch = args.get(1).map(|s| s.as_str());

    // Resolve the path relative to cwd
    let vfs_path = ctx.resolve_path(path_arg);

    // Get real filesystem path
    let real_path = match ctx.backend.resolve_real_path(&vfs_path) {
        Some(p) => p,
        None => {
            // If VFS path doesn't resolve, try resolving the parent
            let parent = vfs_path.parent().unwrap_or(&vfs_path);
            match ctx.backend.resolve_real_path(parent) {
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
fn open_repo(ctx: &ExecContext) -> Result<GitVfs, ExecResult> {
    // Resolve VFS path to real filesystem path
    let real_path = ctx.backend.resolve_real_path(&ctx.cwd).ok_or_else(|| {
        ExecResult::failure(
            128,
            format!(
                "fatal: not a git repository: {} is not on a real filesystem",
                ctx.cwd.display()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::VfsRouter;
    use git2::Repository;
    use std::env;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Arc;
    use tokio::fs;

    static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

    fn temp_dir() -> std::path::PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        env::temp_dir().join(format!("kaish-git-cmd-test-{}-{}", std::process::id(), id))
    }

    async fn setup_git_repo() -> (ExecContext, std::path::PathBuf) {
        let dir = temp_dir();
        let _ = fs::remove_dir_all(&dir).await;
        fs::create_dir_all(&dir).await.unwrap();

        // Initialize git repo
        let repo = Repository::init(&dir).unwrap();
        {
            let mut config = repo.config().unwrap();
            config.set_str("user.name", "Test User").unwrap();
            config.set_str("user.email", "test@example.com").unwrap();
        }

        // Create context with real filesystem at the git repo
        // Mount the temp dir at VFS root "/"
        let mut vfs = VfsRouter::new();
        let local = crate::vfs::LocalFs::new(&dir);
        vfs.mount("/", local);

        let mut ctx = ExecContext::new(Arc::new(vfs));
        // Set CWD to VFS root "/" (which maps to the temp dir)
        ctx.cwd = std::path::PathBuf::from("/");

        (ctx, dir)
    }

    async fn cleanup(dir: &std::path::Path) {
        let _ = fs::remove_dir_all(dir).await;
    }

    #[tokio::test]
    async fn test_git_status_clean() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create initial commit
        fs::write(dir.join("README.md"), b"# Test").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["README.md"]).unwrap();
            git.commit("Initial commit", None).unwrap();
        }

        let args = ToolArgs::new();
        let result = Git.execute(
            {
                let mut a = args;
                a.positional.push(Value::String("status".into()));
                a
            },
            &mut ctx,
        )
        .await;

        assert!(result.ok(), "status failed: {}", result.err);
        assert!(result.out.contains("nothing to commit"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_status_modified() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create and commit initial file
        fs::write(dir.join("test.txt"), b"initial").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["test.txt"]).unwrap();
            git.commit("Initial", None).unwrap();
        }

        // Modify the file
        fs::write(dir.join("test.txt"), b"modified").await.unwrap();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("status".into()));
        args.flags.insert("s".into());

        let result = Git.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("test.txt"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_add_and_commit() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create a file
        fs::write(dir.join("new.txt"), b"new file").await.unwrap();

        // Add the file
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("add".into()));
        args.positional.push(Value::String("new.txt".into()));
        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "add failed: {}", result.err);

        // Commit
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("commit".into()));
        args.named.insert("m".into(), Value::String("Add new file".into()));
        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "commit failed: {}", result.err);
        assert!(result.out.contains("Add new file"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_log() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create a commit
        fs::write(dir.join("file.txt"), b"content").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["file.txt"]).unwrap();
            git.commit("Test commit message", None).unwrap();
        }

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("log".into()));
        args.flags.insert("oneline".into());

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Test commit message"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_log_n_limits_output() {
        // Bug 3: git log -n 3 should only return 3 commits
        let (mut ctx, dir) = setup_git_repo().await;

        // Create 5 commits
        for i in 1..=5 {
            fs::write(dir.join(format!("file{}.txt", i)), format!("content {}", i).as_bytes()).await.unwrap();
            let git = GitVfs::open(&dir).unwrap();
            git.add(&[&format!("file{}.txt", i)]).unwrap();
            git.commit(&format!("Commit {}", i), None).unwrap();
        }

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("log".into()));
        args.flags.insert("oneline".into());
        args.named.insert("count".into(), Value::Int(3));

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "log failed: {}", result.err);
        assert_eq!(result.out.lines().count(), 3, "Expected 3 lines, got: {}", result.out);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_branch() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create initial commit
        fs::write(dir.join("file.txt"), b"content").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["file.txt"]).unwrap();
            git.commit("Initial", None).unwrap();
        }

        // List branches
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("branch".into()));
        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("*")); // Current branch marker

        // Create new branch
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("branch".into()));
        args.named.insert("c".into(), Value::String("feature".into()));
        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "branch -c failed: {}", result.err);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_not_a_repo() {
        let dir = temp_dir();
        let _ = fs::remove_dir_all(&dir).await;
        fs::create_dir_all(&dir).await.unwrap();

        // Create context without git repo
        let mut vfs = VfsRouter::new();
        let local = crate::vfs::LocalFs::new(&dir);
        vfs.mount("/", local);

        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.cwd = dir.clone();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("status".into()));

        let result = Git.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a git repository"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_worktree_list() {
        let (mut ctx, dir) = setup_git_repo().await;

        // Create initial commit (required for worktrees)
        fs::write(dir.join("README.md"), b"# Test").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["README.md"]).unwrap();
            git.commit("Initial commit", None).unwrap();
        }

        // List worktrees
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("worktree".into()));
        args.positional.push(Value::String("list".into()));

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "worktree list failed: {}", result.err);
        // Should show at least the main worktree
        assert!(result.out.contains("(main)") || result.out.contains("master") || !result.out.is_empty());

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_git_worktree_add_and_remove() {
        let (_setup_ctx, dir) = setup_git_repo().await;

        // Create initial commit (required for worktrees)
        fs::write(dir.join("README.md"), b"# Test").await.unwrap();
        {
            let git = GitVfs::open(&dir).unwrap();
            git.add(&["README.md"]).unwrap();
            git.commit("Initial commit", None).unwrap();
        }

        // Create a worktree directory path
        let wt_path = dir.parent().unwrap().join("test-worktree");

        // Mount the parent so we can create the worktree there
        let parent_dir = dir.parent().unwrap().to_path_buf();
        let mut vfs = VfsRouter::new();
        let local = crate::vfs::LocalFs::new(&parent_dir);
        vfs.mount("/", local);
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.cwd = std::path::PathBuf::from("/").join(dir.file_name().unwrap());

        // Add worktree
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("worktree".into()));
        args.positional.push(Value::String("add".into()));
        args.positional.push(Value::String("../test-worktree".into()));

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "worktree add failed: {}", result.err);

        // Verify worktree was created
        assert!(wt_path.exists(), "worktree directory should exist");

        // List should show both worktrees
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("worktree".into()));
        args.positional.push(Value::String("list".into()));

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("test-worktree"));

        // Remove the worktree
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("worktree".into()));
        args.positional.push(Value::String("remove".into()));
        args.positional.push(Value::String("test-worktree".into()));
        args.flags.insert("force".into());

        let result = Git.execute(args, &mut ctx).await;
        assert!(result.ok(), "worktree remove failed: {}", result.err);

        // Cleanup
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }
}
