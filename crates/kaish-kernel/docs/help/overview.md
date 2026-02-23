# kaish (会sh)

Strict Bourne-like shell. No implicit word splitting, no glob expansion, no backticks.
Validates before execution. Builtins run in-process; external commands via PATH fallback.

## Topics

```
help syntax     Variables, quoting, pipes, control flow
help builtins   List of all builtins
help vfs        Virtual filesystem mounts
help scatter    Parallel processing (散/集)
help ignore     Ignore file configuration
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
# Pipes and structured iteration
glob "*.json" | scatter as=F limit=4 | jq ".name" $F | gather

# Variables and conditionals
NAME="world"
if [[ -n $NAME ]]; then echo "Hello, ${NAME}!"; fi

# Background jobs with live observability
cargo build &
cat /v/jobs/1/status     # running | done:0 | failed:N

# MCP tool integration
exa:web_search query="rust async" | jq ".title"
```

## Key Differences from Bash

- No implicit word splitting — use `split` explicitly
- No shell glob expansion — use `glob` builtin or tool-native patterns
- No backticks — use `$(cmd)` always
- ERE regex everywhere — no BRE quirks
- `true`/`false` are commands, not strings — `"true"` is an error in boolean context
