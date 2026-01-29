# kaish Language Reference

Complete reference for kaish syntax and semantics.

## Variables & Data Types

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

## Parameter Expansion

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

## Quoting

```bash
# Double quotes - interpolation works
echo "hello $NAME"
echo "line\nbreak"              # escapes: \n \t \\ \"
echo "literal \$X"              # escaped = no interpolation

# Single quotes - literal strings, no interpolation
echo 'hello $NAME'              # prints: hello $NAME
```

## Arguments

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

## Pipes & Redirects

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

## Statement Chaining

```bash
cmd1 && cmd2                    # run cmd2 only if cmd1 succeeds
cmd1 || cmd2                    # run cmd2 only if cmd1 fails
mkdir /tmp/work && cd /tmp/work && echo "ready"
```

## Test Expressions

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
[[ $filename =~ "\.rs$" ]]      # regex match (quotes allowed, unlike bash)
[[ $input !~ "^[0-9]+$" ]]      # regex not match

# Compound expressions (with short-circuit evaluation)
[[ -f file && -d dir ]]         # logical AND
[[ -z "$VAR" || -n "$DEFAULT" ]] # logical OR
[[ ! -f /tmp/lock ]]            # logical NOT

# Precedence: ! (highest) > && > ||
[[ ! -f a || -d b && -e c ]]    # parsed as: (! -f a) || ((-d b) && (-e c))
```

Note: `[ ]` (single brackets) is not supported — use `[[ ]]` for all tests.

## Control Flow

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

## Command Substitution

```bash
NOW=$(date)
echo "Current time: $NOW"

RESULT=$(cat file.json | jq ".name")
```

## Arithmetic

Arithmetic expansion is **integer-only**. Floats exist as a data type (for JSON interop) but `$(( ))` operates on integers.

```bash
# Arithmetic expansion with $((expression))
X=$((5 + 3))                    # X = 8
Y=$((X * 2))                    # Y = 16
Z=$((10 / 3))                   # Z = 3 (integer division)

# Supported operators (by precedence, highest first)
result=$((-5))                  # unary minus: -5
result=$((10 % 3))              # modulo: 1
result=$((5 * 4 / 2))           # multiply, divide: 10
result=$((10 - 3 + 2))          # add, subtract: 9
result=$(((2 + 3) * 4))         # parentheses: 20

# Comparison operators (return 1 for true, 0 for false)
echo $((5 > 3))                 # 1
echo $((3 >= 3))                # 1
echo $((5 == 5))                # 1
echo $((5 != 3))                # 1
echo $((3 < 5))                 # 1
echo $((3 <= 3))                # 1

# Comparisons have lowest precedence (arithmetic first)
echo $(( (2 + 3) > 4 ))         # 1 (5 > 4)
echo $((10 / 2 == 5))           # 1 (5 == 5)

# Variables in arithmetic
A=10
B=3
echo $((A + B))                 # 13
echo $((A * B + 1))             # 31
echo $((A > B))                 # 1
```

## Error Handling

```bash
set -e                          # exit on first error

some-command || {
    echo "Command failed"
    exit 1
}

source utils.kai                # load utilities
. config.kai                    # dot notation also works
```

## Background Jobs

```bash
slow-task &                     # run in background
jobs                            # list jobs
wait                            # wait for all
wait %1 %2                      # wait for specific
```

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

## 散・集 (San/Shū) — Scatter/Gather

Fan-out parallelism:

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

## MCP Tool Integration

External MCP servers register tools that appear as shell commands with schema-driven CLI translation:

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

**Argument translation (schema-driven):**

| Shell syntax | MCP parameter | Notes |
|--------------|---------------|-------|
| `--query "foo"` | `{"query": "foo"}` | Non-bool schema type → consumes next positional |
| `--limit 10` | `{"limit": 10}` | Type preserved from schema |
| `--verbose` | `{"verbose": true}` | Bool schema type → boolean flag |
| `key="value"` | `{"key": "value"}` | Named arg syntax always works |
| `count=42` | `{"count": 42}` | Type inference from value |

## Virtual Filesystem

VFS mounts provide unified resource access:

```
/                  → kernel root (current working directory)
/scratch/          → in-memory ephemeral storage
/mnt/<name>/       → mounted local paths
/git/              → git repository introspection (status, log, diff, blame)
```

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

## ShellCheck Alignment

**The Bourne-compatible subset of kaish passes `shellcheck --enable=all`.**

Features that ShellCheck warns about (word splitting, glob expansion, backticks) don't exist in kaish.

| SC Code | Warning | Kaish Approach |
|---------|---------|----------------|
| SC2006 | Use `$()` instead of backticks | Backticks don't exist |
| SC2086 | Double quote to prevent word splitting | No word splitting |
| SC2046 | Quote this to prevent word splitting | `$(cmd)` returns single value |
| SC2035 | Use `./*` so globs don't expand | No glob expansion |
| SC2039 | Use `[[ ]]` in POSIX sh | Only `[[ ]]` exists |
| SC1083 | Escape literal braces | No shell-level brace expansion |

## Beyond Bourne

| Feature | POSIX/Bourne | Kaish | Rationale |
|---------|--------------|-------|-----------|
| **Floats** | Integer only | Native `3.14` | MCP tools return JSON with floats |
| **Booleans** | Exit codes | Native `true`/`false` | JSON interop, clearer conditions |
| **Typed params** | None | `name:string` | Tool definitions with validation |
| **Arithmetic** | `$(( ))` | `$((expr))` with comparisons | Integer arithmetic + `>`, `<`, `==` returning 1/0 |
| **Scatter/gather** | None | `散/集` | Built-in parallelism |
| **VFS** | None | `/scratch/`, `/mnt/`, `/git/` | Unified resource access |
| **Pre-validation** | None | `validate` builtin | Catch errors before execution |
| **Strict validation** | Guesses | Rejects `TRUE`, `yes`, `123abc` | Agent-friendly, fail-fast |
