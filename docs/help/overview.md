# kaish (会sh)

Predictable shell for MCP tool orchestration.

- **Bourne-like syntax** — familiar, but no implicit word splitting, glob expansion, or backticks
- **Strict validation** — catches errors before execution, not at runtime
- **Clear errors** — fails fast with useful messages, never guesses
- **Builtin-first** — 54 builtins run in-process; external commands available via PATH fallback

## Topics

```
help syntax     Variables, quoting, pipes, control flow
help builtins   List of 54+ builtins
help vfs        Virtual filesystem mounts
help scatter    Parallel processing (散/集)
help limits     Known limitations
help <tool>     Detailed tool help (e.g., help grep)
```

## Quick Examples

```bash
# External commands work transparently
cargo build --release
git status
date +%Y-%m-%d

# Pipes and filters
ls /l | grep "\.rs$" | head lines=5

# Iteration with glob
for f in $(glob "*.json"); do
    jq ".name" "$f"
done

# Variables and expansion
NAME="world"
echo "Hello, ${NAME}!"
echo "Length: ${#NAME}"

# Conditionals
if [[ -f config.json ]]; then
    jq ".settings" config.json
fi

# Parallel processing
seq 1 10 | scatter as=N limit=4 | echo "processing $N" | gather
```

## Key Differences from Bash

- **No implicit word splitting** — use `split` explicitly
- **No shell glob expansion** — tools handle their own patterns
- **No backticks** — use `$(cmd)` always
- **No `[ ]`** — use `[[ ]]` for all tests
- **ERE regex everywhere** — no BRE quirks

## MCP Tool Integration

External MCP servers register tools as commands:

```bash
exa:web_search query="rust async"
github:create_issue --repo tobert/kaish --title "Bug"
```

## See Also

- LANGUAGE.md — Full language reference
- BUILTINS.md — Detailed builtin documentation
