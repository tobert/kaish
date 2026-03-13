# kaish (会sh)

Bourne-like shell for AI agents. Familiar syntax, fewer footguns.
Validates before execution. Builtins run in-process; external commands via PATH.

## Topics

```
help syntax     Variables, quoting, pipes, control flow
help builtins   List of all builtins
help vfs        Virtual filesystem mounts
help scatter    Parallel processing (散/集)
help ignore     Ignore file configuration
help output-limit  Output size limit configuration
help limits     Known limitations
help <tool>     Detailed tool help (e.g., help grep)
```

## Output Formats

Builtins return structured data. Use `--json` for machine-readable output:

```bash
ls --json                # JSON array of file nodes
kaish-vars --json        # JSON array of {NAME, VALUE} objects
ps --json                # JSON array of process info
```

## Quick Examples

```bash
# Familiar syntax — globs, pipes, control flow all work
ls *.txt | wc -l
for f in src/*.rs; do grep "TODO" "$f"; done

# Variables and conditionals
NAME="world"
if [[ -n $NAME ]]; then echo "Hello, ${NAME}!"; fi

# Parallel execution with scatter/gather
seq 1 100 | scatter as=N limit=4 | process $N | gather

# MCP tool integration
exa:web_search query="rust async" | jq ".title"
```

## Key Differences from Bash

- `$VAR` is always one value — no implicit word splitting (use `split` when needed)
- `*.txt` expands to matching files — zero matches is an error, not a silent pass-through
- No backticks — `$(cmd)` only
- `$(cmd)` returns structured data — `for i in $(seq 1 5)` iterates 5 values, not split text
- ERE regex everywhere — no BRE quirks
- `true`/`false` only — `TRUE`, `yes`, `Yes` are errors
