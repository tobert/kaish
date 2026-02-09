# kaish Known Limitations

Documented limitations of the current implementation.

## Intentionally Missing Features

These bash features are omitted by design:

| Feature | Reason |
|---------|--------|
| Shell brace expansion `{a,b,c}` | Tools handle globs internally |
| Shell glob expansion `*.txt` | Tools handle their own patterns |
| Process substitution `<(cmd)` | Use temp files |
| Backticks `` `cmd` `` | Use `$(cmd)` |
| Single bracket tests `[ ]` | Use `[[ ]]` |
| Aliases, `eval` | Explicit is better |
| Implicit word splitting | Use `split` explicitly |

## Preprocessing

- **Context-unaware** — The preprocessor replaces arithmetic `$(( ))` and heredoc markers before parsing. Escape sequences inside these constructs may behave unexpectedly.

## Validator

- **Lexical scoping** — Static analysis with ShellCheck-like strictness. May warn about variables defined at runtime if not lexically visible. This is intentional.
- **Glob pattern detection** — E013 blocks execution when glob patterns like `"*.txt"` are passed to file-operating commands (ls, cat, rm, etc.). Use `glob "pattern"` for explicit expansion.

## Builtins

| Builtin | Limitation |
|---------|------------|
| `set` | Only `-e` (exit on error) is supported |
| `ps` | Linux-only (reads `/proc`) |
| `git` | Operates on real filesystem, not VFS |
| `head`/`tail -c` | Counts UTF-8 characters, not bytes |

## Execution

- **Scatter results in completion order** — 散 returns results as jobs complete, not in input order.
- **No command substitution in redirect targets** — `cmd > $(...)` not supported. Evaluate the path first.

## External Commands

External commands (via PATH fallback) have constraints:

| Limitation | Reason | Workaround |
|------------|--------|------------|
| No PTY/TTY | Commands run with piped I/O | Use builtins or non-interactive modes |
| Output captured | Not streamed, may be truncated | Large output: redirect to file |
| Virtual cwd fails | `/scratch/` isn't real filesystem | `cd` to real directory first |

```bash
# This works (real cwd)
cd /home/user
cargo build

# This fails (virtual cwd)
cd /scratch
cargo build  # Error: cannot run external from virtual directory
```

## RPC/IPC

- **`spawn_local` requires LocalSet** — IPC clients use tokio's `spawn_local` and must run within a `LocalSet`.
- **RPC shutdown is advisory** — `shutdown()` requests shutdown but doesn't guarantee immediate termination.

## Performance

- **Globstar `**` can be slow** — Deep trees with `**/*.rs` may take time. Use specific prefixes.

## Build/Development

- **chumsky from git** — Parser combinator sourced from git for unreleased features.
- **Fuzz tests require nightly** — Fuzz infrastructure needs Rust nightly.

## Comparison with Bash

| Bash Behavior | kaish Behavior |
|---------------|----------------|
| `for i in $VAR` splits on IFS | No splitting; iterates once |
| `[ -f file ]` | Not supported; use `[[ ]]` |
| `*.txt` expands at shell | Passed literally to tools |
| Regex in `=~` is unquoted | Quotes allowed: `=~ "\.rs$"` |

## Workarounds

**Need word splitting?**
```bash
for i in $(split "$VAR"); do echo $i; done
```

**Need temp file for process substitution?**
```bash
cmd1 > /scratch/temp.txt
cmd2 /scratch/temp.txt
```

**Need glob expansion?**
```bash
for f in $(glob "*.txt"); do echo $f; done
```
