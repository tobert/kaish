//! Git-aware filesystem backend.
//!
//! GitVfs wraps LocalFs with git2 integration, providing:
//! - Standard filesystem operations (delegated to LocalFs)
//! - Git repository state tracking
//! - Git operations (status, add, commit, etc.)
//!
//! # Example
//!
//! ```ignore
//! let git_fs = GitVfs::open("/path/to/repo")?;
//! git_fs.write(Path::new("src/main.rs"), b"new content").await?;
//! let status = git_fs.status()?;
//! ```

use async_trait::async_trait;
use git2::{
    Commit, DiffOptions, Error as GitError, IndexAddOption, Oid, Repository, Signature, Status,
    StatusOptions, StatusShow, WorktreeAddOptions, WorktreeLockStatus, WorktreePruneOptions,
};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use super::local::LocalFs;
use super::traits::{DirEntry, Filesystem, Metadata};

/// Git-aware filesystem backend.
///
/// Wraps LocalFs for file operations while tracking git repository state.
/// All file operations go through the local filesystem, with git operations
/// available through dedicated methods.
pub struct GitVfs {
    /// Underlying local filesystem for file operations.
    local: LocalFs,
    /// Git repository handle.
    repo: Mutex<Repository>,
    /// Root path of the repository.
    root: PathBuf,
}

impl GitVfs {
    /// Open an existing git repository.
    ///
    /// The path should point to a directory containing a `.git` folder
    /// (or the `.git` folder itself for bare repos).
    pub fn open(path: impl Into<PathBuf>) -> Result<Self, GitError> {
        let root: PathBuf = path.into();
        let repo = Repository::open(&root)?;
        let local = LocalFs::new(&root);

        Ok(Self {
            local,
            repo: Mutex::new(repo),
            root,
        })
    }

    /// Clone a repository from a URL.
    pub fn clone(url: &str, path: impl Into<PathBuf>) -> Result<Self, GitError> {
        let root: PathBuf = path.into();
        let repo = Repository::clone(url, &root)?;
        let local = LocalFs::new(&root);

        Ok(Self {
            local,
            repo: Mutex::new(repo),
            root,
        })
    }

    /// Initialize a new git repository.
    pub fn init(path: impl Into<PathBuf>) -> Result<Self, GitError> {
        let root: PathBuf = path.into();
        let repo = Repository::init(&root)?;
        let local = LocalFs::new(&root);

        Ok(Self {
            local,
            repo: Mutex::new(repo),
            root,
        })
    }

    /// Get the root path of the repository.
    pub fn root(&self) -> &Path {
        &self.root
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Status Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Get the current status of the working tree.
    pub fn status(&self) -> Result<Vec<FileStatus>, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut opts = StatusOptions::new();
        opts.include_untracked(true)
            .recurse_untracked_dirs(true)
            .show(StatusShow::IndexAndWorkdir);

        let statuses = repo.statuses(Some(&mut opts))?;
        let mut result = Vec::with_capacity(statuses.len());

        for entry in statuses.iter() {
            if let Some(path) = entry.path() {
                result.push(FileStatus {
                    path: path.to_string(),
                    status: entry.status(),
                });
            }
        }

        Ok(result)
    }

    /// Get a simplified status summary.
    pub fn status_summary(&self) -> Result<StatusSummary, GitError> {
        let statuses = self.status()?;
        let mut summary = StatusSummary::default();

        for file in &statuses {
            if file.status.is_index_new() || file.status.is_index_modified() {
                summary.staged += 1;
            }
            if file.status.is_wt_modified() || file.status.is_wt_new() {
                summary.modified += 1;
            }
            if file.status.is_wt_new() && !file.status.is_index_new() {
                summary.untracked += 1;
            }
        }

        Ok(summary)
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Index Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Add files to the git index (staging area).
    ///
    /// Supports glob patterns (e.g., "*.rs", "src/**/*.rs").
    pub fn add(&self, pathspec: &[&str]) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut index = repo.index()?;

        // Convert pathspecs to owned strings for callback
        let specs: Vec<String> = pathspec.iter().map(|s| s.to_string()).collect();

        index.add_all(
            specs.iter().map(|s| s.as_str()),
            IndexAddOption::DEFAULT,
            None,
        )?;

        index.write()?;
        Ok(())
    }

    /// Add a specific file to the index.
    pub fn add_path(&self, path: &Path) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut index = repo.index()?;
        index.add_path(path)?;
        index.write()?;
        Ok(())
    }

    /// Remove a file from the index (unstage).
    pub fn reset_path(&self, path: &Path) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        // Get HEAD commit
        let head = repo.head()?;
        let head_commit = head.peel_to_commit()?;
        let tree = head_commit.tree()?;

        // Reset the path in the index to match HEAD
        repo.reset_default(Some(head_commit.as_object()), &[path])?;

        // If the file doesn't exist in HEAD, remove it from index
        if tree.get_path(path).is_err() {
            let mut index = repo.index()?;
            let _ = index.remove_path(path);
            index.write()?;
        }

        Ok(())
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Commit Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Create a commit with the staged changes.
    pub fn commit(&self, message: &str, author: Option<&str>) -> Result<Oid, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut index = repo.index()?;
        let tree_oid = index.write_tree()?;
        let tree = repo.find_tree(tree_oid)?;

        // Get or create signature
        let sig = if let Some(author) = author {
            // Parse "Name <email>" format
            if let Some((name, email)) = parse_author(author) {
                Signature::now(&name, &email)?
            } else {
                repo.signature()?
            }
        } else {
            repo.signature()?
        };

        // Get parent commit (if any)
        let parent = match repo.head() {
            Ok(head) => Some(head.peel_to_commit()?),
            Err(_) => None, // First commit
        };

        let parents: Vec<&Commit> = parent.iter().collect();

        let oid = repo.commit(Some("HEAD"), &sig, &sig, message, &tree, &parents)?;

        Ok(oid)
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Log Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Get recent commit log entries.
    pub fn log(&self, count: usize) -> Result<Vec<LogEntry>, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        // Check if HEAD exists (no commits yet returns empty)
        if repo.head().is_err() {
            return Ok(Vec::new());
        }

        let mut revwalk = repo.revwalk()?;
        revwalk.push_head()?;

        let mut entries = Vec::with_capacity(count);

        for (i, oid) in revwalk.enumerate() {
            if i >= count {
                break;
            }

            let oid = oid?;
            let commit = repo.find_commit(oid)?;

            entries.push(LogEntry {
                oid: oid.to_string(),
                short_id: oid.to_string()[..7].to_string(),
                message: commit.message().unwrap_or("").to_string(),
                author: commit.author().name().unwrap_or("").to_string(),
                email: commit.author().email().unwrap_or("").to_string(),
                time: commit.time().seconds(),
            });
        }

        Ok(entries)
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Diff Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Get diff between working tree and HEAD.
    pub fn diff(&self) -> Result<String, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let head = repo.head()?;
        let head_tree = head.peel_to_tree()?;

        let mut opts = DiffOptions::new();
        opts.include_untracked(true);

        let diff = repo.diff_tree_to_workdir_with_index(Some(&head_tree), Some(&mut opts))?;

        let mut output = String::new();
        diff.print(git2::DiffFormat::Patch, |_delta, _hunk, line| {
            let origin = match line.origin() {
                '+' => "+",
                '-' => "-",
                ' ' => " ",
                'H' => "", // Header
                'F' => "", // File header
                'B' => "", // Binary
                _ => "",
            };
            if !origin.is_empty() {
                output.push_str(origin);
            }
            if let Ok(content) = std::str::from_utf8(line.content()) {
                output.push_str(content);
            }
            true
        })?;

        Ok(output)
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Branch Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Get the current branch name.
    pub fn current_branch(&self) -> Result<Option<String>, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        match repo.head() {
            Ok(head) => {
                if head.is_branch() {
                    Ok(head.shorthand().map(|s| s.to_string()))
                } else {
                    // Detached HEAD
                    Ok(None)
                }
            }
            Err(_) => Ok(None), // No commits yet
        }
    }

    /// List all local branches.
    pub fn branches(&self) -> Result<Vec<String>, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let branches = repo.branches(Some(git2::BranchType::Local))?;
        let mut result = Vec::new();

        for branch in branches {
            let (branch, _) = branch?;
            if let Some(name) = branch.name()? {
                result.push(name.to_string());
            }
        }

        Ok(result)
    }

    /// Create a new branch.
    pub fn create_branch(&self, name: &str) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let head = repo.head()?;
        let commit = head.peel_to_commit()?;
        repo.branch(name, &commit, false)?;
        Ok(())
    }

    /// Checkout a branch or commit.
    ///
    /// If `force` is true, overwrites local changes. If false, fails if there
    /// are uncommitted changes that would be overwritten.
    pub fn checkout(&self, target: &str) -> Result<(), GitError> {
        self.checkout_with_options(target, true)
    }

    /// Checkout with explicit force option.
    ///
    /// If `force` is false, checkout will fail if there are uncommitted changes
    /// that would be overwritten by the checkout.
    pub fn checkout_with_options(&self, target: &str, force: bool) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut checkout_opts = git2::build::CheckoutBuilder::new();
        if force {
            checkout_opts.force();
        } else {
            checkout_opts.safe();
        }

        // Try as branch first
        let reference = match repo.find_branch(target, git2::BranchType::Local) {
            Ok(branch) => branch.into_reference(),
            Err(_) => {
                // Try as commit
                let obj = repo.revparse_single(target)?;
                let commit = obj.peel_to_commit()?;
                repo.set_head_detached(commit.id())?;
                repo.checkout_head(Some(&mut checkout_opts))?;
                return Ok(());
            }
        };

        repo.set_head(reference.name().ok_or_else(|| {
            GitError::from_str("invalid reference name")
        })?)?;

        repo.checkout_head(Some(&mut checkout_opts))?;
        Ok(())
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Git Worktree Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// List all worktrees attached to this repository.
    ///
    /// Returns the main worktree plus any linked worktrees.
    pub fn worktrees(&self) -> Result<Vec<WorktreeInfo>, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut result = Vec::new();

        // Add the main worktree (the repository itself)
        let main_path = repo.workdir().unwrap_or(self.root.as_path());
        let head_name = repo
            .head()
            .ok()
            .and_then(|h| h.shorthand().map(String::from));

        result.push(WorktreeInfo {
            name: None, // Main worktree has no name
            path: main_path.to_path_buf(),
            head: head_name,
            locked: false,
            prunable: false,
        });

        // Add linked worktrees
        let worktree_names = repo.worktrees()?;
        for name in worktree_names.iter() {
            if let Some(name) = name {
                if let Ok(wt) = repo.find_worktree(name) {
                    let locked = matches!(wt.is_locked(), Ok(WorktreeLockStatus::Locked(_)));
                    let prunable = wt.is_prunable(None).unwrap_or(false);

                    // Get the HEAD of this worktree by opening it as a repo
                    let wt_head = Repository::open_from_worktree(&wt)
                        .ok()
                        .and_then(|r| {
                            r.head().ok().and_then(|h| h.shorthand().map(String::from))
                        });

                    result.push(WorktreeInfo {
                        name: Some(name.to_string()),
                        path: wt.path().to_path_buf(),
                        head: wt_head,
                        locked,
                        prunable,
                    });
                }
            }
        }

        Ok(result)
    }

    /// Add a new worktree.
    ///
    /// Creates a new worktree at `path` with the given `name`.
    ///
    /// The `committish` parameter accepts:
    /// - Local branch names (`feature`, `main`)
    /// - Remote tracking branches (`origin/main`)
    /// - Commit SHAs (`abc1234`)
    /// - Tags (`v1.0.0`)
    /// - Any git revision (`HEAD~3`)
    ///
    /// If `committish` is None, creates a new branch with the worktree name.
    pub fn worktree_add(
        &self,
        name: &str,
        path: &Path,
        committish: Option<&str>,
    ) -> Result<WorktreeInfo, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut opts = WorktreeAddOptions::new();

        // If a committish is specified, resolve it
        let resolved_ref;
        if let Some(target) = committish {
            // Try as local branch first
            if let Ok(br) = repo.find_branch(target, git2::BranchType::Local) {
                resolved_ref = Some(br.into_reference());
                opts.reference(resolved_ref.as_ref());
            }
            // Try as remote tracking branch (e.g., "origin/main")
            else if let Ok(br) = repo.find_branch(target, git2::BranchType::Remote) {
                resolved_ref = Some(br.into_reference());
                opts.reference(resolved_ref.as_ref());
            }
            // Try as any other reference (tag, commit, HEAD~N, etc.)
            else if let Ok(obj) = repo.revparse_single(target) {
                // For non-branch references, we create the worktree first,
                // then checkout the specific commit (detached HEAD)
                let wt = repo.worktree(name, path, None)?;

                // Open the worktree as a repository and checkout the commit
                let wt_repo = Repository::open_from_worktree(&wt)?;
                let commit = obj.peel_to_commit()?;
                wt_repo.set_head_detached(commit.id())?;

                let mut checkout_opts = git2::build::CheckoutBuilder::new();
                checkout_opts.force();
                wt_repo.checkout_head(Some(&mut checkout_opts))?;

                return Ok(WorktreeInfo {
                    name: Some(name.to_string()),
                    path: wt.path().to_path_buf(),
                    head: Some(commit.id().to_string()[..7].to_string()),
                    locked: false,
                    prunable: false,
                });
            } else {
                return Err(GitError::from_str(&format!(
                    "cannot resolve '{}': not a branch, tag, or commit",
                    target
                )));
            }
        }

        let wt = repo.worktree(name, path, Some(&opts))?;

        // Get info about the newly created worktree
        let locked = matches!(wt.is_locked(), Ok(WorktreeLockStatus::Locked(_)));
        let wt_head = Repository::open_from_worktree(&wt)
            .ok()
            .and_then(|r| {
                r.head().ok().and_then(|h| h.shorthand().map(String::from))
            });

        Ok(WorktreeInfo {
            name: Some(name.to_string()),
            path: wt.path().to_path_buf(),
            head: wt_head,
            locked,
            prunable: false,
        })
    }

    /// Remove a worktree.
    ///
    /// The worktree must not be locked and must be prunable (no uncommitted changes).
    /// Use `force` to remove even if it has changes.
    pub fn worktree_remove(&self, name: &str, force: bool) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let wt = repo.find_worktree(name)?;

        // Check if locked
        if let Ok(WorktreeLockStatus::Locked(reason)) = wt.is_locked() {
            let msg = reason
                .map(|r| format!("worktree '{}' is locked: {}", name, r))
                .unwrap_or_else(|| format!("worktree '{}' is locked", name));
            return Err(GitError::from_str(&msg));
        }

        let mut prune_opts = WorktreePruneOptions::new();
        if force {
            prune_opts.valid(true);
            prune_opts.working_tree(true);
        }

        wt.prune(Some(&mut prune_opts))?;
        Ok(())
    }

    /// Lock a worktree to prevent it from being pruned.
    pub fn worktree_lock(&self, name: &str, reason: Option<&str>) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let wt = repo.find_worktree(name)?;
        wt.lock(reason)?;
        Ok(())
    }

    /// Unlock a worktree.
    pub fn worktree_unlock(&self, name: &str) -> Result<(), GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let wt = repo.find_worktree(name)?;
        wt.unlock()?;
        Ok(())
    }

    /// Prune stale worktree information.
    ///
    /// Removes worktree entries that no longer exist on disk.
    pub fn worktree_prune(&self) -> Result<usize, GitError> {
        let repo = self.repo.lock().map_err(|_| {
            GitError::from_str("failed to acquire repository lock")
        })?;

        let mut pruned = 0;
        let worktree_names = repo.worktrees()?;

        for name in worktree_names.iter() {
            if let Some(name) = name {
                if let Ok(wt) = repo.find_worktree(name) {
                    // Check if the worktree path still exists
                    if wt.validate().is_err() {
                        let mut opts = WorktreePruneOptions::new();
                        if wt.prune(Some(&mut opts)).is_ok() {
                            pruned += 1;
                        }
                    }
                }
            }
        }

        Ok(pruned)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Filesystem Trait Implementation
// ═══════════════════════════════════════════════════════════════════════════

#[async_trait]
impl Filesystem for GitVfs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.local.read(path).await
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.local.write(path, data).await
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        // Filter out .git directory from listings
        let mut entries = self.local.list(path).await?;
        entries.retain(|e| e.name != ".git");
        Ok(entries)
    }

    async fn stat(&self, path: &Path) -> io::Result<Metadata> {
        self.local.stat(path).await
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        self.local.mkdir(path).await
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        self.local.remove(path).await
    }

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.local.rename(from, to).await
    }

    fn read_only(&self) -> bool {
        self.local.read_only()
    }
}

impl std::fmt::Debug for GitVfs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GitVfs")
            .field("root", &self.root)
            .finish()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper Types
// ═══════════════════════════════════════════════════════════════════════════

/// Information about a worktree.
#[derive(Debug, Clone)]
pub struct WorktreeInfo {
    /// Name of the worktree (None for main worktree).
    pub name: Option<String>,
    /// Path to the worktree directory.
    pub path: PathBuf,
    /// Current HEAD reference (branch name or commit).
    pub head: Option<String>,
    /// Whether the worktree is locked.
    pub locked: bool,
    /// Whether the worktree can be pruned.
    pub prunable: bool,
}

/// Status of a single file in the working tree.
#[derive(Debug, Clone)]
pub struct FileStatus {
    /// Path relative to repository root.
    pub path: String,
    /// Git status flags.
    pub status: Status,
}

impl FileStatus {
    /// Get a human-readable status character (like git status --porcelain).
    pub fn status_char(&self) -> &'static str {
        if self.status.is_index_new() {
            "A "
        } else if self.status.is_index_modified() {
            "M "
        } else if self.status.is_index_deleted() {
            "D "
        } else if self.status.is_wt_modified() {
            " M"
        } else if self.status.is_wt_new() {
            "??"
        } else if self.status.is_wt_deleted() {
            " D"
        } else {
            "  "
        }
    }
}

/// Summary of repository status.
#[derive(Debug, Clone, Default)]
pub struct StatusSummary {
    /// Number of staged files.
    pub staged: usize,
    /// Number of modified files (not staged).
    pub modified: usize,
    /// Number of untracked files.
    pub untracked: usize,
}

/// A single log entry (commit).
#[derive(Debug, Clone)]
pub struct LogEntry {
    /// Full commit OID.
    pub oid: String,
    /// Short (7-char) commit ID.
    pub short_id: String,
    /// Commit message.
    pub message: String,
    /// Author name.
    pub author: String,
    /// Author email.
    pub email: String,
    /// Commit timestamp (Unix seconds).
    pub time: i64,
}

/// Parse "Name <email>" format.
fn parse_author(s: &str) -> Option<(String, String)> {
    if let Some(lt_pos) = s.find('<') {
        if let Some(gt_pos) = s.find('>') {
            let name = s[..lt_pos].trim().to_string();
            let email = s[lt_pos + 1..gt_pos].trim().to_string();
            return Some((name, email));
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::sync::atomic::{AtomicU64, Ordering};
    use tokio::fs;

    static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

    fn temp_dir() -> PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        env::temp_dir().join(format!("kaish-git-test-{}-{}", std::process::id(), id))
    }

    async fn setup_repo() -> (GitVfs, PathBuf) {
        let dir = temp_dir();
        let _ = fs::remove_dir_all(&dir).await;
        fs::create_dir_all(&dir).await.unwrap();

        // Configure git identity for tests
        let repo = Repository::init(&dir).unwrap();
        {
            let mut config = repo.config().unwrap();
            config.set_str("user.name", "Test User").unwrap();
            config.set_str("user.email", "test@example.com").unwrap();
        }

        let git_fs = GitVfs {
            local: LocalFs::new(&dir),
            repo: Mutex::new(repo),
            root: dir.clone(),
        };

        (git_fs, dir)
    }

    async fn cleanup(dir: &Path) {
        let _ = fs::remove_dir_all(dir).await;
    }

    #[tokio::test]
    async fn test_init_and_write() {
        let (git_fs, dir) = setup_repo().await;

        // Write a file
        git_fs
            .write(Path::new("test.txt"), b"hello git")
            .await
            .unwrap();

        // Verify file was written
        let content = git_fs.read(Path::new("test.txt")).await.unwrap();
        assert_eq!(content, b"hello git");

        // Check status shows untracked file
        let status = git_fs.status().unwrap();
        assert_eq!(status.len(), 1);
        assert_eq!(status[0].path, "test.txt");
        assert!(status[0].status.is_wt_new());

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_add_and_commit() {
        let (git_fs, dir) = setup_repo().await;

        // Write and add a file
        git_fs
            .write(Path::new("test.txt"), b"hello git")
            .await
            .unwrap();
        git_fs.add(&["test.txt"]).unwrap();

        // Verify file is staged
        let status = git_fs.status().unwrap();
        assert_eq!(status.len(), 1);
        assert!(status[0].status.is_index_new());

        // Commit
        let oid = git_fs.commit("Initial commit", None).unwrap();
        assert!(!oid.is_zero());

        // Verify clean status after commit
        let status = git_fs.status().unwrap();
        assert!(status.is_empty());

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_log() {
        let (git_fs, dir) = setup_repo().await;

        // Create a commit
        git_fs
            .write(Path::new("test.txt"), b"content")
            .await
            .unwrap();
        git_fs.add(&["test.txt"]).unwrap();
        git_fs.commit("Test commit", None).unwrap();

        // Check log
        let log = git_fs.log(10).unwrap();
        assert_eq!(log.len(), 1);
        assert!(log[0].message.contains("Test commit"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_branch_operations() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("test.txt"), b"content")
            .await
            .unwrap();
        git_fs.add(&["test.txt"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Get current branch
        let branch = git_fs.current_branch().unwrap();
        assert!(branch.is_some()); // Should be master or main

        // Create new branch
        git_fs.create_branch("feature").unwrap();

        // List branches
        let branches = git_fs.branches().unwrap();
        assert!(branches.len() >= 2);
        assert!(branches.contains(&"feature".to_string()));

        // Checkout feature branch
        git_fs.checkout("feature").unwrap();
        let branch = git_fs.current_branch().unwrap();
        assert_eq!(branch, Some("feature".to_string()));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_list_hides_git_dir() {
        let (git_fs, dir) = setup_repo().await;

        // Write a file
        git_fs
            .write(Path::new("test.txt"), b"content")
            .await
            .unwrap();

        // List should not include .git
        let entries = git_fs.list(Path::new("")).await.unwrap();
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"test.txt"));
        assert!(!names.contains(&".git"));

        cleanup(&dir).await;
    }

    #[test]
    fn test_parse_author() {
        assert_eq!(
            parse_author("John Doe <john@example.com>"),
            Some(("John Doe".to_string(), "john@example.com".to_string()))
        );
        assert_eq!(parse_author("invalid"), None);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Worktree Tests
    // ═══════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_worktrees_lists_main() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit (required for worktrees)
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // List worktrees - should have main worktree
        let worktrees = git_fs.worktrees().unwrap();
        assert_eq!(worktrees.len(), 1);
        assert!(worktrees[0].name.is_none()); // Main worktree has no name
        assert!(worktrees[0].head.is_some()); // Should have a branch

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_add_with_new_branch() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create worktree path
        let wt_path = dir.parent().unwrap().join("test-wt-new");

        // Add worktree (no branch specified = new branch)
        let info = git_fs.worktree_add("test-wt", &wt_path, None).unwrap();
        assert_eq!(info.name, Some("test-wt".to_string()));
        assert!(info.path.exists());

        // List should now show 2 worktrees
        let worktrees = git_fs.worktrees().unwrap();
        assert_eq!(worktrees.len(), 2);

        // Cleanup
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_add_with_existing_branch() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create a branch
        git_fs.create_branch("feature").unwrap();

        // Create worktree for existing branch
        let wt_path = dir.parent().unwrap().join("test-wt-branch");
        let info = git_fs.worktree_add("wt-feature", &wt_path, Some("feature")).unwrap();

        assert_eq!(info.name, Some("wt-feature".to_string()));
        assert!(info.path.exists());

        // Cleanup
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_add_with_commit() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        let oid = git_fs.commit("Initial commit", None).unwrap();

        // Create second commit
        git_fs
            .write(Path::new("README.md"), b"# Updated")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Second commit", None).unwrap();

        // Create worktree at specific commit (first commit)
        let wt_path = dir.parent().unwrap().join("test-wt-commit");
        let short_oid = &oid.to_string()[..7];
        let info = git_fs.worktree_add("wt-commit", &wt_path, Some(short_oid)).unwrap();

        assert_eq!(info.name, Some("wt-commit".to_string()));
        assert!(info.path.exists());
        // HEAD should be the short commit id (detached)
        assert!(info.head.is_some());

        // Cleanup
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_add_invalid_ref() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Try to create worktree with invalid ref
        let wt_path = dir.parent().unwrap().join("test-wt-invalid");
        let result = git_fs.worktree_add("wt-bad", &wt_path, Some("nonexistent-branch"));

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message().contains("cannot resolve"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_lock_unlock() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create worktree
        let wt_path = dir.parent().unwrap().join("test-wt-lock");
        git_fs.worktree_add("wt-lock", &wt_path, None).unwrap();

        // Lock it
        git_fs.worktree_lock("wt-lock", Some("testing")).unwrap();

        // Verify locked in list
        let worktrees = git_fs.worktrees().unwrap();
        let locked_wt = worktrees.iter().find(|w| w.name.as_deref() == Some("wt-lock"));
        assert!(locked_wt.is_some());
        assert!(locked_wt.unwrap().locked);

        // Unlock it
        git_fs.worktree_unlock("wt-lock").unwrap();

        // Verify unlocked
        let worktrees = git_fs.worktrees().unwrap();
        let unlocked_wt = worktrees.iter().find(|w| w.name.as_deref() == Some("wt-lock"));
        assert!(!unlocked_wt.unwrap().locked);

        // Cleanup
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_remove() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create worktree
        let wt_path = dir.parent().unwrap().join("test-wt-remove");
        git_fs.worktree_add("wt-remove", &wt_path, None).unwrap();

        // Verify it exists
        assert_eq!(git_fs.worktrees().unwrap().len(), 2);

        // Remove it (force because it's valid)
        git_fs.worktree_remove("wt-remove", true).unwrap();

        // Verify removed from list
        assert_eq!(git_fs.worktrees().unwrap().len(), 1);

        // Cleanup (path may or may not exist after remove)
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_remove_locked_fails() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create and lock worktree
        let wt_path = dir.parent().unwrap().join("test-wt-locked-rm");
        git_fs.worktree_add("wt-locked", &wt_path, None).unwrap();
        git_fs.worktree_lock("wt-locked", None).unwrap();

        // Try to remove - should fail
        let result = git_fs.worktree_remove("wt-locked", false);
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("locked"));

        // Cleanup
        git_fs.worktree_unlock("wt-locked").unwrap();
        git_fs.worktree_remove("wt-locked", true).unwrap();
        let _ = fs::remove_dir_all(&wt_path).await;
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_worktree_prune() {
        let (git_fs, dir) = setup_repo().await;

        // Create initial commit
        git_fs
            .write(Path::new("README.md"), b"# Test")
            .await
            .unwrap();
        git_fs.add(&["README.md"]).unwrap();
        git_fs.commit("Initial commit", None).unwrap();

        // Create worktree
        let wt_path = dir.parent().unwrap().join("test-wt-prune");
        git_fs.worktree_add("wt-prune", &wt_path, None).unwrap();

        // Verify 2 worktrees
        assert_eq!(git_fs.worktrees().unwrap().len(), 2);

        // Manually delete the worktree directory (simulating stale state)
        fs::remove_dir_all(&wt_path).await.unwrap();

        // Prune should clean up the stale entry
        let pruned = git_fs.worktree_prune().unwrap();
        assert_eq!(pruned, 1);

        // Should be back to 1 worktree
        assert_eq!(git_fs.worktrees().unwrap().len(), 1);

        cleanup(&dir).await;
    }
}
