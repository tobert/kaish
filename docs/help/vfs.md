# kaish Virtual Filesystem (VFS)

The VFS provides unified resource access across different backends.

## Mount Points

```
/              kernel root (current working directory)
/scratch/      in-memory ephemeral storage
/tmp/          in-memory temporary storage
/mnt/local/    user's home directory ($HOME)
/git/          git repository introspection
```

## /mnt/local — Local Filesystem

Maps to the user's `$HOME` directory. This is where you'll find your projects, config files, etc.

```bash
ls /mnt/local                    # list home directory
cat /mnt/local/.bashrc           # read a file
ls /mnt/local/src/myproject      # navigate subdirectories
```

## /scratch — Ephemeral Memory

In-memory storage for temporary data. Fast, but lost when session ends.

```bash
echo "temp data" > /scratch/cache.txt
cat /scratch/cache.txt           # read it back
```

## /tmp — Temporary Storage

Another in-memory location, following Unix conventions.

```bash
write /tmp/temp.json '{"key": "value"}'
jq '.key' /tmp/temp.json
```

## /git — Repository Introspection

Read-only access to git metadata for the current working directory.

```bash
cat /git/status                  # git status output
cat /git/log                     # recent commits
cat /git/diff                    # current diff
cat /git/blame/path/to/file.rs  # blame for specific file
```

## Path Conventions

- Absolute paths start with `/`
- Paths are Unix-style (forward slashes)
- The kernel root `/` is the current working directory
- Use `/mnt/local` to access user's home directory

## Example Session

```bash
# Start in /mnt/local by default
pwd                              # /mnt/local

# Work with local files
ls src/
cat src/main.rs | head -20

# Use scratch for intermediate data
ls src/*.rs | write /scratch/rust_files.txt

# Check git status
cat /git/status
```

## Notes

- The VFS is per-session; changes to `/scratch` don't persist
- `/mnt/local` provides real filesystem access
- File operations use VFS paths, not native OS paths
