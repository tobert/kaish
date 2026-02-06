# kaish (会sh)

**A predictable shell for AI agents** — Bourne-like syntax without the gotchas.

The 会 (kai) means "gathering" in Japanese. Part of [Kaijutsu](https://github.com/tobert/kaijutsu) (会術) — the art of gathering.

## Why kaish?

Traditional shells have evolved syntax with many sharp edges. kaish implements the
commonly-used parts of sh while eliminating entire classes of bugs at the language level:

- **No implicit word splitting** — `$VAR` is always one value, never split on spaces
- **No glob expansion** — tools handle their own patterns, or use `glob` builtin
- **Structured iteration** — `for i in $(seq 1 5)` works via structured data, not word splitting
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

for i in $(seq 1 3); do            # structured data iteration
    echo "Count: $i"
done

for file in $(glob "*.txt"); do    # structured data iteration
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
| **66 builtins** | grep, jq, git, find, sed, awk, diff, patch, and more |
| **Structured data** | Commands return typed arrays — `for i in $(seq 1 5)` iterates 5 values, not word-split text |
| **Strict validation** | Errors caught before execution with clear messages |
| **Virtual filesystem** | Unified access: `/mnt/local` (home), `/scratch` (memory), `/v/jobs` (observability) |
| **Scatter/gather** | Built-in parallelism with 散/集 |

See [Language Reference](docs/LANGUAGE.md) for complete syntax. Use `help builtins` or `help <tool>` for per-tool docs.

---

## Builtins

kaish ships 66 builtins that run in-process — no subprocesses, no PATH lookups, no platform
variance. They exist because agents need tools they can verify: a `grep` that behaves identically
everywhere, a `jq` that always uses the same filter syntax, an `awk` that never surprises.

**Design principles:**

- **Verifiable** — each builtin has a schema (params, types, examples) exposed via `help <tool>`.
  Agents can introspect before calling.
- **Convention-following** — flags and behavior match the patterns deeply embedded in training data
  and decades of existing scripts. `grep -rn`, `sed 's/old/new/g'`, `awk '{print $1}'` all work
  as expected.
- **80/20** — implement the features used 80% of the time, deliberately omit the 20% that add
  complexity without proportional value. Missing features compose via pipes.
- **ERE everywhere** — all regex uses Extended Regular Expressions. No BRE/ERE confusion.

| Category | Tools |
|----------|-------|
| **Text** | awk, cut, grep, head, sed, sort, split, tail, tr, uniq, wc |
| **Files** | basename, cat, cd, chmod, cp, dirname, find, glob, ln, ls, mkdir, mktemp, mv, pwd, read, readlink, realpath, rm, stat, tee, touch, tree, write |
| **JSON** | jq |
| **Git** | git (init, clone, status, add, commit, log, diff, branch, checkout, worktree) |
| **System** | date, echo, env, exec, export, help, hostname, jobs, kill, printf, ps, seq, set, sleep, test/\[\[, tokens, uname, unset, validate, vars, wait, which |
| **Parallel** | scatter, gather |
| **Meta** | assert, diff, false, mounts, patch, tools, true |

---

## Components

kaish is built as a set of crates that can be used independently:

### kaish-kernel

The core execution engine. Lexer, parser, interpreter, builtins, VFS.

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
Supports: pipes, redirects, here-docs, if/for/while, functions, builtins,
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

Build from source:

```bash
git clone https://github.com/tobert/kaish
cd kaish
cargo build --release
```

## License

MIT
