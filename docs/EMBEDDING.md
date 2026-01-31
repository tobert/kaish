# Embedding kaish

This guide shows how to embed kaish in your application, providing shell
scripting capabilities with access to VFS, git operations, and the full
builtin toolkit.

## Quick Start

```rust
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create a kernel with defaults
    let kernel = Kernel::transient()?;

    // Execute shell commands
    let result = kernel.execute("echo 'Hello from kaish!'").await?;
    println!("{}", result.out);

    Ok(())
}
```

## Architecture

kaish separates concerns into layers:

```text
┌─────────────────────────────────────────────────────────┐
│  Your Application (e.g., kaijutsu)                       │
├─────────────────────────────────────────────────────────┤
│  KernelBackend trait                                     │
│  - resolve_real_path() → maps VFS paths to real paths   │
│  - File operations, tool dispatch, mounts               │
├─────────────────────────────────────────────────────────┤
│  Kernel                                                  │
│  - Lexer/Parser/Interpreter                             │
│  - Tool Registry (54 builtins)                          │
│  - VFS Router                                           │
└─────────────────────────────────────────────────────────┘
```

## Custom Backend for Git Operations

The key to getting git operations "for free" is implementing `resolve_real_path()`.
This method tells kaish how to map VFS paths to real filesystem paths where
git repositories live.

### Example: kaijutsu-style Worktrees

```rust
use std::path::{Path, PathBuf};
use std::sync::Arc;
use kaish_kernel::{
    Kernel, KernelConfig, KernelBackend, LocalBackend,
    xdg_data_home, GitVfs,
};

/// Custom backend that maps VFS paths to kaijutsu worktrees
struct KaijutsuBackend {
    /// Delegate to LocalBackend for file operations
    inner: LocalBackend,
    /// Root of worktrees directory
    worktrees_root: PathBuf,
}

impl KaijutsuBackend {
    fn new() -> Self {
        let worktrees_root = xdg_data_home()
            .join("kaijutsu")
            .join("worktrees");

        Self {
            inner: LocalBackend::new(),
            worktrees_root,
        }
    }
}

impl KernelBackend for KaijutsuBackend {
    // ... delegate most methods to self.inner ...

    /// Map VFS paths to real worktree paths
    fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        // /mnt/repos/kaish/src/main.rs → ~/.local/share/kaijutsu/worktrees/kaish/src/main.rs
        if let Ok(rest) = path.strip_prefix("/mnt/repos") {
            return Some(self.worktrees_root.join(rest));
        }

        // Other mounts...
        None
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let backend = Arc::new(KaijutsuBackend::new());
    let config = KernelConfig::default();

    let kernel = Kernel::with_backend(backend, config)?;

    // Git operations now work on worktrees!
    kernel.execute("cd /mnt/repos/kaish && git status").await?;

    Ok(())
}
```

### How Git Operations Work

When you run `git status` in kaish:

1. The `git` builtin receives the current working directory (e.g., `/mnt/repos/kaish`)
2. It calls `backend.resolve_real_path(&cwd)`
3. Your backend returns the real path (e.g., `~/.local/share/kaijutsu/worktrees/kaish`)
4. kaish opens a `GitVfs` at that real path
5. Git operations work directly on the worktree

## Direct GitVfs Access

For lower-level git operations, use `GitVfs` directly:

```rust
use kaish_kernel::{GitVfs, FileStatus, LogEntry, WorktreeInfo};
use std::path::Path;

fn inspect_repo() -> anyhow::Result<()> {
    let repo = GitVfs::open("/path/to/worktree")?;

    // Get current branch
    if let Some(branch) = repo.current_branch()? {
        println!("On branch: {}", branch);
    }

    // Check status
    let status = repo.status()?;
    for file in &status {
        println!("{} {}", file.status_char(), file.path);
    }

    // Stage and commit
    repo.add(&["src/*.rs"])?;
    repo.commit("Update source files", None)?;

    // View log
    for entry in repo.log(5)? {
        println!("{} {}", entry.short_id, entry.message.lines().next().unwrap_or(""));
    }

    Ok(())
}

fn manage_worktrees() -> anyhow::Result<()> {
    let repo = GitVfs::open("/path/to/main/repo")?;

    // List all worktrees
    for wt in repo.worktrees()? {
        println!("{}: {} ({:?})",
            wt.name.as_deref().unwrap_or("main"),
            wt.path.display(),
            wt.head
        );
    }

    // Create a new worktree for a feature branch
    let wt_info = repo.worktree_add(
        "feature-work",
        Path::new("/path/to/feature-worktree"),
        Some("feature-branch"),  // existing branch, or None for new branch
    )?;
    println!("Created worktree at {}", wt_info.path.display());

    // Lock a worktree to prevent accidental pruning
    repo.worktree_lock("feature-work", Some("work in progress"))?;

    // Later, unlock and remove
    repo.worktree_unlock("feature-work")?;
    repo.worktree_remove("feature-work", false)?;  // force=false

    // Clean up stale worktree entries
    let pruned = repo.worktree_prune()?;
    println!("Pruned {} stale worktree(s)", pruned);

    Ok(())
}
```

## Path Composition with XDG Primitives

kaish exports XDG base directory primitives so embedders can compose
their own application-specific paths:

```rust
use kaish_kernel::paths::{
    xdg_data_home,    // ~/.local/share or $XDG_DATA_HOME
    xdg_config_home,  // ~/.config or $XDG_CONFIG_HOME
    xdg_cache_home,   // ~/.cache or $XDG_CACHE_HOME
    xdg_runtime_dir,  // $XDG_RUNTIME_DIR or /tmp
    home_dir,         // ~ or $HOME
};

// Compose your own paths
fn myapp_data_dir() -> PathBuf {
    xdg_data_home().join("myapp")
}

fn myapp_worktrees_dir() -> PathBuf {
    myapp_data_dir().join("worktrees")
}

fn myapp_repos_dir() -> PathBuf {
    myapp_data_dir().join("repos")
}

// Or use completely custom paths
fn custom_storage() -> PathBuf {
    PathBuf::from("/opt/myapp/storage")
}
```

## Tilde Expansion

For user-facing path handling, use `expand_tilde`:

```rust
use kaish_kernel::expand_tilde;

let path = expand_tilde("~/projects/myrepo");
// → /home/username/projects/myrepo
```

## Exported Types

The `kaish_kernel` crate exports these types at the crate root for convenience:

### Core Types
- `Kernel` — The execution engine
- `KernelConfig` — Configuration options
- `KernelBackend` — Trait for custom backends
- `LocalBackend` — Default filesystem backend

### Git Types
- `GitVfs` — Git-aware filesystem
- `FileStatus` — Status of a single file
- `StatusSummary` — Aggregate status counts
- `LogEntry` — A commit in the log
- `WorktreeInfo` — Information about a worktree

### Job Observability
- `BoundedStream` — Ring-buffer stream for capturing command output (10MB default)
- `StreamStats` — Statistics about a bounded stream
- `drain_to_stream()` — Drain async reader into bounded stream
- `DEFAULT_STREAM_MAX_SIZE` — Default max size constant (10MB)
- `JobFs` — VFS backend for observing job state at `/v/jobs`

### Path Primitives
- `home_dir()` — User's home directory
- `xdg_data_home()` — XDG data directory
- `xdg_config_home()` — XDG config directory
- `xdg_cache_home()` — XDG cache directory
- `xdg_runtime_dir()` — XDG runtime directory

### Utilities
- `expand_tilde()` — Expand `~` in paths

## Configuration Options

`KernelConfig` provides these options:

```rust
let config = KernelConfig {
    // Mount local filesystem at /mnt/local
    mount_local: true,

    // Root for local mount (defaults to $HOME)
    local_root: Some(PathBuf::from("/custom/root")),

    // Initial working directory
    cwd: PathBuf::from("/mnt/local"),

    // Skip pre-execution validation (for performance)
    skip_validation: false,
};
```

## Job Output Capture

kaish provides bounded streams for capturing command output without OOM risk.

### BoundedStream for Custom Output Capture

Use `BoundedStream` when you need to capture process output with memory bounds:

```rust
use kaish_kernel::{BoundedStream, drain_to_stream, DEFAULT_STREAM_MAX_SIZE};
use std::sync::Arc;
use tokio::process::Command;

async fn capture_with_bounds() -> anyhow::Result<String> {
    let mut child = Command::new("some-chatty-command")
        .stdout(std::process::Stdio::piped())
        .spawn()?;

    // Create bounded stream (10MB max, oldest data evicted on overflow)
    let stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

    // Drain stdout into the bounded stream
    if let Some(stdout) = child.stdout.take() {
        let stream_clone = stream.clone();
        tokio::spawn(async move {
            drain_to_stream(stdout, stream_clone).await;
        });
    }

    child.wait().await?;

    // Read captured output (safe even if process wrote gigabytes)
    Ok(stream.read_string().await)
}
```

### JobFs for Background Job Observability

The kernel automatically mounts `JobFs` at `/v/jobs`, exposing background job state:

```
/v/jobs/
├── 1/
│   ├── stdout    # Captured stdout (bounded)
│   ├── stderr    # Captured stderr (bounded)
│   └── status    # "running", "completed:0", or "failed:1"
├── 2/
│   └── ...
```

Access job output via shell or VFS:

```bash
# In kaish scripts
sleep 10 &              # Starts job 1
jobs                    # Shows: [1] running  /v/jobs/1/
cat /v/jobs/1/status    # "running"

# After completion
cat /v/jobs/1/stdout    # Job's stdout
cat /v/jobs/1/stderr    # Job's stderr
```

Or programmatically via the kernel's VFS:

```rust
let kernel = Kernel::new(KernelConfig::default())?;

// Run background job
kernel.execute("long_running_command &").await?;

// Later, check job output via VFS
let stdout = kernel.vfs().read("/v/jobs/1/stdout").await?;
let status = kernel.vfs().read("/v/jobs/1/status").await?;
```

## Best Practices

1. **Use `resolve_real_path()`** — This is the key abstraction. Map your VFS
   paths to real paths where git repos live.

2. **Compose paths with XDG primitives** — Don't hardcode paths. Use the
   XDG functions and compose your application-specific paths on top.

3. **Use `with_backend()`** — For full control, implement `KernelBackend`
   and create the kernel with `Kernel::with_backend()`.

4. **Direct `GitVfs` for complex operations** — For operations beyond
   what the `git` builtin provides, use `GitVfs` directly.

5. **Handle worktrees vs bare repos** — The `git` builtin works on worktrees
   (real files). If you use bare repos internally, map VFS paths to worktree
   paths, not bare repo paths.
