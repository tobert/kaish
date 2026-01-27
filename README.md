# kaish (会sh)

**An embeddable shell for AI agents** — 80% of POSIX sh, 100% unambiguous.

The 会 (kai) means "gathering" in Japanese. Part of [Kaijutsu](https://github.com/tobert/kaijutsu) (会術) — the art of gathering.

## Why kaish?

Traditional shells like bash are interpreted line-by-line and have evolved syntax
with many sharp edges. Kaish attempts to implement the most commonly used parts of
sh, dropping several features that are easy to make mistakes with.

**kaish eliminates entire classes of bugs at the language level:**

- No word splitting — `$VAR` is always one value
- No glob expansion — tools handle globbing or use glob() builtin
- No backticks — only `$(cmd)` substitution
- Strict booleans — `TRUE` and `yes` are errors, not truthy
- ShellCheck-clean — the Bourne subset passes `shellcheck --enable=all`

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

for item in $ITEMS; do
    echo "Processing: $item"
done

# Pipes and redirects
cat urls.txt | grep "https" | head -10 > filtered.txt

# MCP tools look like CLI commands
exa:web_search --query "rust parser combinators"

# Expand glob patterns to file paths
glob **/*.rs --exclude='*_test.rs'

# Parallel execution with scatter/gather
cat items.txt | scatter as=ITEM limit=4 | process $ITEM | gather
```

## Key Features

| Feature | Description |
|---------|-------------|
| **Bourne-compatible** | Variables, pipes, control flow, functions — familiar syntax |
| **50+ builtins** | ls, grep, find, jq, git, diff, patch, and more |
| **MCP integration** | External tools appear as commands with schema-driven CLI translation |
| **Virtual filesystem** | Unified access to local files, memory, and git repos |
| **Scatter/gather** | Built-in parallelism with 散/集 |
| **Pre-validation** | Catch errors before execution with `validate` |
| **Embeddable** | Library-first design for integration into larger systems |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Frontends                            │
│   REPL  ·  Script Runner  ·  Kaijutsu / Embedded           │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                   会sh 核 (Kernel)                          │
│                                                             │
│  Lexer (logos) → Parser (chumsky) → Interpreter (tokio)    │
│                                                             │
│  ┌─────────────┐ ┌─────────────┐ ┌───────────────────────┐ │
│  │  Variables  │ │    Tools    │ │    VFS Router         │ │
│  │  & Scope    │ │  (54 built- │ │  /mnt, /scratch, /git │ │
│  │             │ │   in + MCP) │ │                       │ │
│  └─────────────┘ └─────────────┘ └───────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

Frontends connect via **embedded** (in-process) or **IPC** (Unix sockets + Cap'n Proto).

## Documentation

| Document | Description |
|----------|-------------|
| [Language Reference](docs/LANGUAGE.md) | Complete syntax and semantics |
| [Builtins](docs/BUILTINS.md) | All 54 builtin tools |
| [Grammar](docs/GRAMMAR.md) | Formal EBNF grammar |
| [Architecture](docs/ARCHITECTURE.md) | Kernel design and protocols |
| [漢字 Reference](docs/kanji.md) | Kanji vocabulary |

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

