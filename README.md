# kaish (会sh)

**A predictable shell for AI agents** — Bourne-like syntax without the gotchas.

The 会 (kai) means "gathering" in Japanese. Part of [Kaijutsu](https://github.com/tobert/kaijutsu) (会術) — the art of gathering.

## Why kaish?

Traditional shells have evolved syntax with many sharp edges. kaish implements the
commonly-used parts of sh while eliminating entire classes of bugs at the language level:

- **No implicit word splitting** — `$VAR` is always one value, never split on spaces
- **No glob expansion** — tools handle their own patterns, or use `glob` builtin
- **Structured iteration** — `for i in $(seq 1 5)` works because `seq` returns a JSON array
- **Explicit splitting** — use `split "$VAR"` when you actually need word splitting
- **No backticks** — only `$(cmd)` substitution
- **Strict booleans** — `TRUE` and `yes` are errors, not truthy
- **Pre-validation** — catch errors before execution, not at runtime

Skills transfer from bash. Footguns don't.

## Quick Tour

```bash
#!/usr/bin/env kaish

# Familiar bash-style syntax
GREETING="Hello"
echo "$GREETING, world!"

# Control flow
if [[ -f config.json ]]; then
    echo "Config found"
fi

# For loops - no implicit word splitting!
for item in one two three; do      # literal items
    echo "Processing: $item"
done

for i in $(seq 1 3); do            # seq returns array
    echo "Count: $i"
done

for file in $(glob "*.txt"); do    # glob returns array
    echo "Found: $file"
done

# Pipes and redirects
cat urls.txt | grep "https" | head -10 > filtered.txt

# Expand glob patterns
glob "**/*.rs" --exclude="*_test.rs"

# Parallel execution with scatter/gather
seq 1 10 | scatter as=N limit=4 | echo "processing $N" | gather
```

## Language Features

| Feature | Description |
|---------|-------------|
| **Bourne-compatible** | Variables, pipes, control flow, functions — familiar syntax |
| **54 builtins** | grep, jq, git, find, sed, awk, diff, patch, and more |
| **Strict validation** | Errors caught before execution with clear messages |
| **Virtual filesystem** | Unified access: `/mnt/local` (home), `/scratch` (memory), `/git` |
| **Scatter/gather** | Built-in parallelism with 散/集 |

See [Language Reference](docs/LANGUAGE.md) for complete syntax and [Builtins](docs/BUILTINS.md) for all 54 tools.

---

## Components

kaish is built as a set of crates that can be used independently:

### kaish-kernel

The core execution engine. Lexer, parser, interpreter, 54 builtins, VFS.

```rust
use kaish_kernel::{Kernel, KernelConfig};

let kernel = Kernel::new(KernelConfig::default())?;
let result = kernel.execute("echo hello | tr a-z A-Z").await?;
println!("{}", result.out);  // "HELLO"
```

The kernel is embeddable — no external dependencies, no subprocess spawning for builtins.

### kaish-repl

Interactive shell with readline support, history, and tab completion.

```bash
$ kaish
kaish> for f in $(glob "*.rs"); do wc -l "$f"; done
  142 main.rs
   87 lib.rs
kaish>
```

### kaish-mcp

MCP server exposing kaish as tools for AI agents.

#### Installation

Add to your MCP client configuration:

```json
{
  "mcpServers": {
    "kaish": {
      "command": "kaish-mcp"
    }
  }
}
```

#### Tools

**`execute`** — Run kaish scripts in a fresh, isolated environment.

```
Supports: pipes, redirects, here-docs, if/for/while, functions, 54 builtins,
${VAR:-default}, $((arithmetic)), scatter/gather parallelism.

NOT supported: process substitution <(), backticks, eval, aliases.

Paths: /mnt/local = $HOME, /scratch/ = ephemeral memory.
```

**`help`** — Discover syntax, builtins, VFS, and capabilities.

```
Topics: overview, syntax, builtins, vfs, scatter, limits
Tool help: help grep, help jq, help git
```

#### Why an MCP shell?

AI agents need to compose operations — filter outputs, transform data, iterate over results.
Raw MCP tools are individual operations; kaish lets agents combine them:

```bash
# Filter and transform in one script
ls /mnt/local/src | grep "\.rs$" | head -5

# Iterate over results
for f in $(glob "*.json"); do
    jq ".name" "$f"
done

# Parallel processing
seq 1 10 | scatter as=N limit=4 | echo "processing $N" | gather
```

The kernel runs builtins in-process (no exec), making it fast and predictable.

#### MCP Client Mode

kaish can also consume external MCP tools, appearing as namespaced commands:

```bash
# External MCP tools look like CLI commands
exa:web_search --query "rust parser combinators"

# Pipe MCP results through kaish builtins
github:list_issues --repo="foo/bar" | jq '.[] | .title'
```

---

## Installation

```bash
cargo install kaish
```

Or build from source:

```bash
git clone https://github.com/tobert/kaish
cd kaish
cargo build --release
```

## License

MIT
