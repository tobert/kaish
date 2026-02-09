# kaish Virtual Filesystem (VFS)

The VFS provides unified resource access across different backends.

## Mount Modes

kaish supports different VFS configurations depending on the use case:

### Passthrough Mode (REPL)

For human-operated REPL sessions, native paths work directly:

```
/                native filesystem root
/v/              memory storage for blobs
/scratch/        ephemeral in-memory storage
```

Example:
```bash
ls /home/atobey/src/kaish     # works directly
cat /etc/passwd                # works
pwd                            # shows actual cwd
```

### Sandboxed Mode (MCP)

For agent/MCP use, paths look native but access is restricted to `$HOME`:

```
/                memory root (catches paths outside sandbox)
/home/user/      user's home directory (sandboxed)
/tmp/            in-memory temporary storage
/v/              memory storage for blobs
/scratch/        ephemeral in-memory storage
```

Example:
```bash
ls /home/atobey/src/kaish     # works (within $HOME)
cat /etc/passwd                # fails (outside sandbox)
pwd                            # /home/atobey
```

The sandbox can be restricted further (e.g., to `~/src`) via configuration.

## Path Conventions

- Absolute paths start with `/`
- Paths are Unix-style (forward slashes)
- In passthrough mode, all paths work
- In sandboxed mode, only paths under the sandbox root work

## /scratch — Ephemeral Memory

In-memory storage for temporary data. Fast, but lost when session ends.

```bash
echo "temp data" > /scratch/cache.txt
cat /scratch/cache.txt         # read it back
```

## /tmp — Temporary Storage

Another in-memory location, following Unix conventions.

```bash
write /tmp/temp.json '{"key": "value"}'
jq '.key' /tmp/temp.json
```

## /v — Virtual Namespace

Memory storage for blobs and other synthetic resources.

```bash
# Blobs are stored at /v/blobs/{id}
ls /v/blobs/
```

### /v/jobs — Job Observability

Live access to background job state. Each job gets a directory:

```
/v/jobs/
└── {job_id}/
    ├── stdout   ← live output stream (ring buffer, 10MB max)
    ├── stderr   ← live error stream
    ├── status   ← "running" | "done:0" | "failed:N"
    └── command  ← the original command string
```

Example:
```bash
# Start background job
cargo build &
# [1] Running cargo build  /v/jobs/1/

# Check progress
cat /v/jobs/1/status    # running
cat /v/jobs/1/stdout    # build output so far

# After completion
cat /v/jobs/1/status    # done:0

# Cleanup completed jobs
jobs --cleanup
```

Output streams use bounded ring buffers (10MB default). If a job produces more output, the oldest data is evicted — the pipe never blocks and memory stays bounded.

## /git — Repository Introspection

Read-only access to git metadata for the current working directory.

```bash
cat /git/status                # git status output
cat /git/log                   # recent commits
cat /git/diff                  # current diff
cat /git/blame/path/to/file.rs # blame for specific file
```

## Example Session (MCP/Sandboxed)

```bash
# Start in $HOME by default
pwd                            # /home/atobey

# Work with local files using native paths
cd /home/atobey/src/kaish
ls src/
cat src/main.rs | head -20

# Use scratch for intermediate data
ls src/*.rs | write /scratch/rust_files.txt

# Check git status
cat /git/status

# This fails - outside sandbox
cat /etc/passwd                # error: not found
```

## Example Session (REPL/Passthrough)

```bash
# Native paths work directly
pwd                            # /home/atobey/src/kaish
ls src/
cat /etc/passwd                # full filesystem access
```

## Security Implications

| Mode | Access | Use Case |
|------|--------|----------|
| Passthrough | Full filesystem | Human-operated REPL |
| Sandboxed | `$HOME` only (or subset) | Agents, MCP servers |
| NoLocal | Memory only | Tests, isolation |

**Passthrough mode** gives full filesystem access — appropriate for human users who trust their own commands.

**Sandboxed mode** restricts access to the user's home directory (or a configured subset). Paths look native but `/etc/passwd` is not accessible.
