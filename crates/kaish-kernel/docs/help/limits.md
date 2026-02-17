# kaish Known Limitations

## Intentionally Missing

| Feature | Workaround |
|---------|------------|
| Shell glob expansion `*.txt` | `glob "*.txt"` or tool-native patterns |
| Shell brace expansion `{a,b,c}` | List items explicitly |
| Process substitution `<(cmd)` | `cmd > /tmp/t.txt; cmd2 /tmp/t.txt` |
| Backticks `` `cmd` `` | `$(cmd)` |
| `eval` | Write explicit code |
| Implicit word splitting | `split "$VAR"` |

`[ ]` is supported as a builtin but `[[ ]]` is preferred.

## Builtin Constraints

| Builtin | Limitation |
|---------|------------|
| `alias` | First word only; not in pipelines or compound commands |
| `set` | Only `-e` (exit on error) |
| `ps` | Linux-only (reads `/proc`) |
| `git` | Operates on real filesystem, not VFS |
| `head`/`tail -c` | Counts UTF-8 characters, not bytes |
| `**` globs | Slow on deep trees; use specific prefixes |

## Execution

- **Pipeline stages run concurrently** with isolated scopes (like bash subshells). Variable assignments in one stage aren't visible in others. Last stage syncs back to parent.
- **User functions and .kai scripts cannot run inside pipeline stages or scatter workers.** Only builtins and external commands work in these contexts.
- **Scatter results in completion order**, not input order.
- **No command substitution in redirect targets** — `cmd > $(...)` not supported. Evaluate path first.
- **Preprocessor is context-unaware** — `$(( ))` and heredoc markers replaced before parsing.

## External Commands

| Constraint | Workaround |
|-----------|------------|
| No PTY assumed | TTY works if present, but kaish doesn't allocate one |
| Output buffered (non-pipeline) | Redirect to file or use in pipeline |
| Virtual cwd fails | `cd` to real directory before running |
| Bypass VFS sandbox | External binaries access the real filesystem |

## Bash vs kaish

| Bash | kaish |
|------|-------|
| `for i in $VAR` splits on IFS | No splitting; iterates once |
| `*.txt` expands at shell | Passed literally to tools |
| Regex in `=~` is unquoted | Quotes allowed: `=~ "\.rs$"` |
| `printf "a"; printf "b"` → `ab` | → `a\nb` (line-separated, intentional) |
