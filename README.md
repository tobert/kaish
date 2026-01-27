# kaish (会sh)

kaish is an embeddable 80/20 implementation of a POSIX shell and core utils.

The kai in kaish is 会, which is kanji for a meeting, gathering, or coming together in Japanese.

Part of the [Kaijutsu](https://github.com/tobert/kaijutsu) (会術) project — the art of gathering.

## Quick Tour

```bash
#!/usr/bin/env kaish

# Variables - bash style
GREETING="Hello"
CONFIG='{"host": "localhost", "port": 8080}'

# Both $VAR and ${VAR} work
echo "$GREETING, world!"
echo "Config: ${CONFIG}"

# Control flow
if [[ -f config.json ]]; then
    echo "Config found"
elif [[ -d /etc/kaish ]]; then
    echo "System config exists"
else
    echo "No config"
fi

# Loops
for item in $ITEMS; do
    echo "Processing: $item"
done

# Parameter expansion
NAME=${NAME:-"default"}      # default value
echo "Length: ${#NAME}"      # string length

# MCP tools look like CLI commands (schema-driven translation)
exa:web_search --query "rust parser combinators"

# File finding with globstar
files **/*.rs                           # find all Rust files
files src/**/*.go --exclude='*_test.go' # exclude test files
tree src/                               # compact: src/{main.rs,lib/{mod.rs,utils.rs}}
grep -rn "TODO" src/                    # recursive search with line numbers

# Functions
greet() {
    echo "Hello, $1!"
}
greet "World"  # → Hello, World!

# 散/集 (san/shū) — scatter/gather parallelism
# These are built in alternatives to xargs
# Experimental, might change or be removed
cat urls.txt | scatter as=URL limit=4 | curl $URL | gather > results.json

```

---

## Language Reference

### Variables & Data Types

```bash
# Assignment - bash style (no spaces around =)
NAME="value"
local NAME="value"              # local scope

# Data types
COUNT=42                        # integer
PI=3.14159                      # float (extension)
ENABLED=true                    # boolean (extension)
ITEMS="one two three"           # string (space-separated for iteration)
DATA='{"key": "value"}'         # JSON stored as string

# Both $VAR and ${VAR} work
echo $NAME
echo ${NAME}
echo "${NAME} more text"
```

**Why floats?** MCP tools return JSON, which has floats. Kaish supports them natively.

**Why strict booleans?** Only `true`/`false` are valid. `TRUE`, `Yes`, `1` are errors — catches AI generation mistakes early.

### Parameter Expansion

```bash
NAME=${NAME:-"default"}         # use "default" if NAME unset or empty
echo ${#NAME}                   # string length

# Positional parameters
echo $0                         # script/tool name
echo $1 $2 $3                   # first three args
echo $@                         # all args
echo $#                         # arg count
echo $?                         # exit code of last command (0-255)
```

### Quoting

```bash
# Double quotes - interpolation works
echo "hello $NAME"
echo "line\nbreak"              # escapes: \n \t \\ \"
echo "literal \$X"              # escaped = no interpolation

# Single quotes - literal strings, no interpolation
echo 'hello $NAME'              # prints: hello $NAME
```

### Arguments

```bash
# Positional
echo "hello" "world"

# Named (key=value, no spaces)
tool arg1="value" count=10 enabled=true

# Flags
ls -l                           # short flag
ls -la                          # combined short flags
git commit -m "message"         # short flag with value
git push --force                # long flag
curl --header="Content-Type: json"  # long flag with value
```

### Pipes & Redirects

```bash
tool-a | tool-b | tool-c        # pipe stdout → stdin
tool > file                     # redirect stdout
tool >> file                    # append stdout
tool < file                     # stdin from file
tool 2> file                    # redirect stderr
tool &> file                    # stdout + stderr
tool 2>&1                       # merge stderr into stdout
cmd 2>&1 | tee log.txt          # capture both streams

# Here-docs
cat <<EOF
multi-line
content here
EOF
```

### Statement Chaining

```bash
cmd1 && cmd2                    # run cmd2 only if cmd1 succeeds
cmd1 || cmd2                    # run cmd2 only if cmd1 fails
mkdir /tmp/work && cd /tmp/work && echo "ready"
```

### Test Expressions

```bash
# File tests
[[ -f /path/file ]]             # is file
[[ -d /path/dir ]]              # is directory
[[ -e /path/any ]]              # exists
[[ -r /path/file ]]             # readable
[[ -w /path/file ]]             # writable
[[ -x /path/file ]]             # executable

# String tests
[[ -z $VAR ]]                   # empty
[[ -n $VAR ]]                   # non-empty

# Comparisons
[[ $X == "value" ]]             # equality
[[ $X != "other" ]]             # inequality
[[ $NUM -gt 5 ]]                # greater than
[[ $NUM -lt 10 ]]               # less than
[[ $NUM -ge 5 ]]                # greater or equal
[[ $NUM -le 10 ]]               # less or equal
[[ $filename =~ "\.rs$" ]]      # regex match
[[ $input !~ "^[0-9]+$" ]]      # regex not match
```

Note: `[ ]` (single brackets) is not supported — use `[[ ]]` for all tests.

### Control Flow

```bash
# Conditional
if CONDITION; then
    ...
elif OTHER_CONDITION; then
    ...
else
    ...
fi

# For loop
for ITEM in "one two three"; do
    echo $ITEM
done

# While loop
while CONDITION; do
    ...
done

# Case statement
case $VAR in
    hello) echo "matched hello" ;;
    "*.rs") echo "Rust file" ;;
    "y"|"yes") echo "yes" ;;
    "*") echo "default" ;;
esac

# Control statements
break                           # exit innermost loop
break 2                         # exit 2 levels
continue                        # skip to next iteration
return                          # return from tool
return 1                        # return with exit code
exit                            # exit script
exit 1                          # exit with code
```

### Command Substitution

```bash
NOW=$(date)
echo "Current time: $NOW"

RESULT=$(cat file.json | jq ".name")
```

### Arithmetic

```bash
# Arithmetic expansion with $((expression))
X=$((5 + 3))                    # X = 8
Y=$((X * 2))                    # Y = 16
Z=$((10 / 3))                   # Z = 3 (integer division)

# Supported operators (by precedence, highest first)
result=$((2 ^ 3))               # exponentiation: 8
result=$((10 % 3))              # modulo: 1
result=$((5 * 4 / 2))           # multiply, divide: 10
result=$((10 - 3 + 2))          # add, subtract: 9
result=$((-5))                  # unary minus: -5
result=$(((2 + 3) * 4))         # parentheses: 20

# Variables in arithmetic
A=10
B=3
echo $((A + B))                 # 13
echo $((A * B + 1))             # 31
```

### Error Handling

```bash
set -e                          # exit on first error

some-command || {
    echo "Command failed"
    exit 1
}

source utils.kai                # load utilities
. config.kai                    # dot notation also works
```

### Background Jobs

```bash
slow-task &                     # run in background
jobs                            # list jobs
wait                            # wait for all
wait %1 %2                      # wait for specific
```

---

## 散・集 (San/Shū) — Scatter/Gather (experimental)

Fan-out parallelism made easy:

```bash
# 散 (scatter) - fan out to parallel workers
# 集 (gather) - collect results back
cat items.txt | scatter as=ITEM limit=8 | process $ITEM | gather > results.json

# With progress
cat big_list.txt \
    | scatter as=ID limit=4 \
    | slow-operation id=$ID \
    | gather progress=true
```

## Virtual Filesystem

VFS mounts are configured programmatically via the kernel API. The router uses longest-prefix matching to resolve paths to the correct backend.

```
/                  → kernel root (current working directory)
/scratch/          → in-memory ephemeral storage
/mnt/<name>/       → mounted local paths
/git/              → git repository introspection (status, log, diff, blame)
```

---

## MCP Tool Integration

External MCP servers can register their tools with the kernel. Tools appear as shell
commands with **schema-driven CLI translation** — use familiar `--flag value` syntax:

```bash
# MCP tools use server:tool naming convention
exa:web_search --query "kaish shell language"
filesystem:read_file --path /etc/hosts

# Schema-aware: --flag consumes next arg as value (non-bool params)
github:create_issue --repo tobert/kaish --title "Bug report" --body "Details"

# Or use named args directly
exa:web_search query="rust async" limit=5

# Boolean flags work too
myserver:deploy --verbose --dry-run

# Pipe MCP tool output like any command
exa:web_search --query "rust" | jq '.results[0].url'
```

**How it works:**
- MCP servers are registered at kernel startup
- Each tool is prefixed with its server name: `{server}:{tool}`
- Tool schemas are introspected to enable CLI-style argument parsing
- JSON responses flow through pipes like any other output
- Tools appear alongside builtins in `tools` listing

**Argument translation (schema-driven):**
| Shell syntax | MCP parameter | Notes |
|--------------|---------------|-------|
| `--query "foo"` | `{"query": "foo"}` | Non-bool schema type → consumes next positional |
| `--limit 10` | `{"limit": 10}` | Type preserved from schema |
| `--verbose` | `{"verbose": true}` | Bool schema type → boolean flag |
| `key="value"` | `{"key": "value"}` | Named arg syntax always works |
| `count=42` | `{"count": 42}` | Type inference from value |

---

## Functions

Shell-style functions using positional parameters:

```bash
# POSIX-style: name() { body }
greet() {
    echo "Hello, $1!"
}
greet "Amy"  # → Hello, Amy!

# Bash-style: function name { body }
function count_args {
    echo "Got $# arguments: $@"
}
count_args a b c  # → Got 3 arguments: a b c
```

Positional parameters: `$0` (function name), `$1`-`$9` (args), `$@` (all args), `$#` (count)

Functions execute in **shared scope** (sh-compatible) — they can read and modify parent variables. Use `local` for function-local variables.

### Script Execution via PATH

Scripts with `.kai` extension can be called by name when in a `PATH` directory:

```bash
# Create a script
write "/scripts/fetch.kai" 'echo "Fetching $1..."'

# Add to PATH
PATH="/scripts:/bin"

# Call by name (without .kai extension)
fetch "example.com"  # → Fetching example.com...
```

Scripts execute in **isolated scope** (like a subshell) — they cannot access or modify parent variables.

### Scope Summary

| Method | Scope | Can modify parent vars? |
|--------|-------|------------------------|
| `myfunc args` | shared | ✓ yes |
| `source script.kai` | shared | ✓ yes |
| `scriptname` (via PATH) | isolated | ✗ no |

---

## Builtin Tools

### Core

| Tool | Description |
|------|-------------|
| `echo` | Output text |
| `printf` | Formatted output |
| `read` | Read from stdin |
| `cd` | Change directory |
| `pwd` | Print working directory |
| `set` | Set shell options (`set -e`) |
| `unset` | Unset variables |
| `export` | Export variables to environment |
| `env` | Access environment variables |
| `source`/`.` | Load and execute scripts |
| `exec` | Execute external command |
| `true` | Exit with code 0 |
| `false` | Exit with code 1 |
| `test`/`[` | POSIX conditional expressions |
| `help` | Tool documentation |

### Files & Directories

| Tool | Description |
|------|-------------|
| `ls` | List directory (`-R` for recursive) |
| `cat` | Read file |
| `head` | First N lines |
| `tail` | Last N lines |
| `write` | Write to file |
| `tee` | Read stdin, write to file and stdout |
| `touch` | Create file / update timestamp |
| `stat` | File metadata |
| `mkdir` | Create directory |
| `rm` | Remove file/directory |
| `cp` | Copy files |
| `mv` | Move/rename files |
| `mktemp` | Create temp file or directory |
| `files` | Find files with glob patterns (`files **/*.rs`) |
| `find` | Find files with predicates (`-name`, `-type`, `-mtime`) |
| `tree` | Directory tree (compact brace notation by default) |

### Paths

| Tool | Description |
|------|-------------|
| `basename` | Strip directory from path |
| `dirname` | Get directory part of path |
| `realpath` | Resolve to absolute path |
| `readlink` | Read symlink target |
| `which` | Find command in PATH |

### Text Processing

| Tool | Description |
|------|-------------|
| `grep` | Search content (`-r`/`-R` recursive, `--include`/`--exclude`) |
| `sed` | Stream editor for transformations |
| `awk` | Pattern scanning and text processing |
| `cut` | Extract fields/columns |
| `tr` | Translate characters |
| `sort` | Sort lines |
| `uniq` | Filter duplicate lines |
| `wc` | Count lines/words/chars |
| `diff` | Compare files (context/unified diff output) |
| `patch` | Apply diff patches |
| `jq` | JSON query (native implementation via jaq) |

### Utilities

| Tool | Description |
|------|-------------|
| `seq` | Generate number sequences |
| `sleep` | Delay execution |
| `date` | Current timestamp |
| `assert` | Test assertions |

### Jobs & Parallelism

| Tool | Description |
|------|-------------|
| `jobs` | List background jobs |
| `wait` | Wait for jobs |
| `scatter` | 散 — Parallel fan-out |
| `gather` | 集 — Collect parallel results |

### Git

| Tool | Description |
|------|-------------|
| `git` | Git operations via git2 (status, log, diff, blame) |

### Introspection

| Tool | Description |
|------|-------------|
| `vars` | List variables |
| `tools` | List available tools |
| `mounts` | List VFS mounts |
| `history` | Show execution history |
| `checkpoints` | List checkpoints |
| `introspect` | Tool schema introspection |
| `validate` | Pre-execution script validation |

---

## What's Intentionally Missing

These bash features are omitted because they're confusing, error-prone, or ambiguous:

| Feature | Reason | ShellCheck |
|---------|--------|------------|
| Shell brace expansion `echo {a,b,c}` | Tools support globs with braces internally | SC1083 |
| Shell glob expansion `*.txt` | Tools handle their own patterns | SC2035 |
| Process substitution `<(cmd)` | Use temp files | — |
| Backtick substitution `` `cmd` `` | Use `$(cmd)` | SC2006 |
| Single bracket tests `[ ]` | Use `[[ ]]` | SC2039 |
| Aliases, `eval` | Explicit is better | SC2091 |

---

## ShellCheck Alignment

**The Bourne-compatible subset of kaish passes `shellcheck --enable=all`.**

Features that ShellCheck warns about (word splitting, glob expansion, backticks) don't exist in kaish. This eliminates entire classes of bugs at the language level.

### Warnings Impossible to Trigger

| SC Code | Warning | Kaish Approach |
|---------|---------|----------------|
| SC2006 | Use `$()` instead of backticks | Backticks don't exist |
| SC2086 | Double quote to prevent word splitting | No word splitting |
| SC2046 | Quote this to prevent word splitting | `$(cmd)` returns single value |
| SC2035 | Use `./*` so globs don't expand | No glob expansion |
| SC2039 | Use `[[ ]]` in POSIX sh | Only `[[ ]]` exists |
| SC1083 | Escape literal braces | No shell-level brace expansion (tools handle internally) |

### Why This Matters

**For humans:** Skills transfer from bash. Features ShellCheck warns against don't exist.

**For AI agents:** Can't generate word-splitting bugs. Strict validation catches mistakes early.

**Extensions are explicit:** Floats, typed params, scatter/gather, MCP tools — clearly distinct syntax, outside ShellCheck's scope.

---

## Beyond Bourne

| Feature | POSIX/Bourne | Kaish | Rationale |
|---------|--------------|-------|-----------|
| **Floats** | Integer only | Native `3.14` | MCP tools return JSON with floats |
| **Booleans** | Exit codes | Native `true`/`false` | JSON interop, clearer conditions |
| **Typed params** | None | `name:string` | Tool definitions with validation |
| **Arithmetic** | `$(( ))` | `$((expr))` with `^` for exponent | Full arithmetic with proper precedence |
| **Scatter/gather** | None | `散/集` | Built-in parallelism |
| **VFS** | None | `/scratch/`, `/mnt/`, `/git/` | Unified resource access |
| **Pre-validation** | None | `validate` builtin | Catch errors before execution |
| **Strict validation** | Guesses | Rejects `TRUE`, `yes`, `123abc` | Agent-friendly, fail-fast |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                          Frontends                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────────┐  │
│  │    REPL     │  │   Script    │  │     Kaijutsu / Embedded     │  │
│  │             │  │   Runner    │  │                             │  │
│  └──────┬──────┘  └──────┬──────┘  └─────────────┬───────────────┘  │
└─────────┼────────────────┼───────────────────────┼──────────────────┘
          │                │                       │
          └────────────────┴───────┬───────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    会sh 核 (Kaku) — Kernel                          │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ State: variables, tool definitions, VFS mounts, job handles   │ │
│  └────────────────────────────────────────────────────────────────┘ │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐   │
│  │    Lexer     │  │    Parser    │  │       Interpreter        │   │
│  │   (logos)    │  │   (chumsky)  │  │   (async, tokio-based)   │   │
│  └──────────────┘  └──────────────┘  └──────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

The 核 (kaku/kernel) is the unit of execution. Frontends connect via:
- **Embedded** — direct in-process (for Kaijutsu)
- **IPC** — Unix sockets with Cap'n Proto RPC

State is persisted in SQLite (WAL mode) for crash recovery.

## Documentation

- [Formal Grammar](docs/GRAMMAR.md) — EBNF, ambiguity analysis
- [Architecture](docs/ARCHITECTURE.md) — 核 design, crate structure, protocols
- [漢字 Reference](docs/kanji.md) — kanji vocabulary for the project

## Schema

- [`schema/kaish.capnp`](schema/kaish.capnp) — Cap'n Proto schema (kernel protocol, types)
- [`schema/state.sql`](schema/state.sql) — SQLite schema (kernel state persistence)

## License

MIT

---

*"The gathering shell" — because orchestrating AI tools should feel like conducting a symphony, not wrestling with syntax.*
