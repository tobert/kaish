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
//! ```

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
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
                "Git subcommand (init, clone, status, add, commit, log, diff, branch, checkout)",
            ))
            .param(ParamSchema::optional(
                "args",
                "array",
                Value::Null,
                "Additional arguments for the subcommand",
            ))
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
        Ok(_) => ExecResult::success(format!(
            "Initialized empty Git repository in {}",
            vfs_path.display()
        )),
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
        Ok(_) => ExecResult::success(format!("Cloning into '{}'...\ndone.", vfs_dest.display())),
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
                    return ExecResult::success("");
                }
                return ExecResult::success("nothing to commit, working tree clean");
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

            ExecResult::success(output.trim_end())
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
        Ok(()) => ExecResult::success(""),
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
            ExecResult::success(format!("[{short}] {message}"))
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

    // Get count (-n flag)
    let count = args
        .get_named("n")
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
                return ExecResult::success("No commits yet");
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

            ExecResult::success(output.trim_end())
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
        Ok(diff) => ExecResult::success(diff.trim_end()),
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
                            return ExecResult::success(format!("Switched to a new branch '{}'", name))
                        }
                        Err(e) => return ExecResult::failure(1, format!("git checkout: {}", e)),
                    }
                }
                ExecResult::success(format!("Branch '{}' created", name))
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

                ExecResult::success(output.trim_end())
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
        Ok(()) => ExecResult::success(format!("Switched to '{}'", target)),
        Err(e) => ExecResult::failure(1, format!("git checkout: {}", e)),
    }
}

/// Open the git repository in the current working directory.
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
}
