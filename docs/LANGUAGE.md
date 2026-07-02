# kaish Language Reference

Complete reference for kaish syntax and semantics.

## Variables & Data Types

```sh
# Assignment - bash style (no spaces around =)
NAME="value"
local NAME="value"              # local scope

# Data types
COUNT=42                        # integer
PI=3.14159                      # float (extension)
ENABLED=true                    # boolean (extension)
ITEMS="one two three"           # string (space-separated for iteration)
DATA='{"key": "value"}'         # JSON stored as string
KEY=$(dd if=/dev/urandom bs=16 count=1)   # binary bytes (see Binary Data)

# Both $VAR and ${VAR} work
echo $NAME
echo ${NAME}
echo "${NAME} more text"
```

**Why floats?** JSON data often has floats. Kaish supports them natively for easy interop.

**Why strict booleans?** Only `true`/`false` are valid. `TRUE`, `Yes`, `1` are errors — catches AI generation mistakes early.

### Inline environment prefix — `NAME=value command`

One or more assignments placed *before* a command scope those variables to that
command only — they reach the command (its arguments and, for external commands,
its subprocess environment) and are **removed afterward**, matching bash:

```sh
RUST_LOG=debug cargo run        # RUST_LOG set for cargo only
A=1 B=2 ./script                # both scoped to ./script
echo "$RUST_LOG"                # empty — the prefix did not persist
```

A bare assignment with **no** command following (`NAME=value` alone, or
`NAME=value; …`) is a normal persistent assignment, as before. (kaish divergence:
the prefixed variable is visible to the command's own argument expansion, e.g.
`FOO=bar echo $FOO` prints `bar`, because kaish builtins read from scope; bash
expands the args first and prints empty.)

## Parameter Expansion

```sh
NAME=${NAME:-"default"}         # use "default" if NAME unset or empty
echo ${#NAME}                   # string length

# Positional parameters
echo $0                         # script/tool name
echo $1 $2 $3                   # first three args
echo $@                         # all args
echo $#                         # arg count
echo $?                         # exit code of last command (0-255)
```

## Collections (Lists & Records)

Values can be structured JSON — a **list** or a **record** — not only strings.
`fromjson` / `tojson` bridge JSON text in and out; `$(cmd)` that emits structured
data (`jq`, `keys`, `values`, `seq`, …) yields a typed value directly.

```sh
u=$(fromjson '{"name":"amy","tags":["rust","shell"]}')
xs=$(fromjson '[10,20,30]')
```

### Read access — brackets only, never dots

```sh
${u[name]}          # record key — a bareword is a LITERAL key
${u[$k]}            # dynamic key — $var is evaluated
${u["a key"]}       # quoted key (spaces / punctuation)
${xs[0]}            # list index (0-based)
${xs[-1]}           # negative index counts from the end
${xs[0:2]}          # slice — end-exclusive, yields a list
${u[tags][0]}       # nested path
${#xs}   ${#u}      # length: list element count / record key count
${#u[tags]}         # length of a nested value (path-aware)
```

A subscript that lands on a JSON **scalar** unwraps to a native value, so
comparisons and arithmetic are typed (not stringly):

```sh
c=$(fromjson '{"port":8080,"healthy":false}')
echo $(( ${c[port]} + 1 ))                       # 8081  (integer arithmetic)
[[ ${c[healthy]} == false ]] && echo "down"      # typed bool
xs=$(fromjson '[10,20,30]'); i=1; echo $(( xs[i] ))   # bare subscript reads VARIABLE i → 20
```

Inside `$(( … ))` a bare subscript is a numeric expression, so `xs[i]` reads the
**variable** `i` — the opposite of the interpolation form `${xs[i]}`, where a
bareword is a literal key. Write `${xs[$i]}` for a variable key in interpolation.

**Default with `:-`.** `${path:-default}` yields the default on **absence** — an
unset root, a missing key, an out-of-bounds index — and on an **empty** value
(JSON `null` or empty string), but never on a present-but-falsy value:

```sh
cfg=$(fromjson '{"port":9000}')
echo ${cfg[port]:-8080}       # 9000  (present)
echo ${cfg[host]:-localhost}  # localhost  (missing key → default)
echo ${cfg[flag]:-on}         # if flag is false/0/[]/{} you get that value, not "on"
```

A **shape** error (integer index on a record, string key on a list, subscripting
a scalar, dotted access) stays loud even with `:-` — misuse is not absence.

Every bad access is a **loud error**, never a silent empty — dotted access
(`${u.name}` → use `[name]`), a missing record key, an out-of-bounds index, a
string key on a list, an integer index on a record, and subscripting a scalar.

### keys / values — iterate any collection

`keys` and `values` turn a collection into a list you iterate with `$(...)`.
They work on **both** shapes (the jq model):

```sh
keys $u             # record → its keys:    ["name","tags"]
values $u           # record → its values
keys $xs            # list   → its indices:  [0,1,2]
values $xs          # list   → its elements: [10,20,30]
```

### Iteration is `$()`-only

There is no word splitting, so a bare `$var`/`${...}` in a for-head is an error
(**E012**) — including a subscript access, which is still a variable reference.
Wrap the collection in `keys`/`values`:

```sh
for x in $(values $xs); do echo $x; done              # each element: 10 20 30
for i in $(keys $xs);   do echo $i; done              # each index:   0 1 2
for k in $(keys $u);    do echo "$k = ${u[$k]}"; done # key + value
for t in $(values ${u[tags]}); do echo $t; done       # nested list
```

### Membership — `in` / `not in`

The right-hand side must be a collection (a string RHS is a loud error — use
`=~` / globs / `case` for substrings):

```sh
[[ rust in $(values ${u[tags]}) ]]   # element present in a list?
[[ name in $u ]]                     # record has key?
[[ 1 in $(keys $xs) ]]               # index in bounds?
[[ tmp not in $u ]]                  # absent?
```

Element equality uses the same rule as `==` (a numeric bareword matches a JSON
number). Comparing a whole collection to a scalar with `==`/`!=` is a loud error
— test membership with `in`, or compare structures with `jq`.

### Shape guards — `typeof` / `[[ -list ]]` / `[[ -record ]]`

An API call sometimes returns a list, sometimes a single record — the shape
guard is the antidote: check the shape *before* committing to `keys`/`values`
or a `for` loop over it.

```sh
typeof $x            # "list" | "record" | "string" | "number" | "bool" | "null" | "bytes"
t=$(typeof $x)        # capture the type name
[[ -list $x ]]        # true iff $x is a list
[[ -record $x ]]      # true iff $x is a record

# The guard idiom:
if [[ -record $data ]]; then
  for k in $(keys $data); do echo "$k = ${data[$k]}"; done
elif [[ -list $data ]]; then
  for x in $(values $data); do echo $x; done
fi
```

`typeof` never splits int/float — both are `number`, matching jq/JSON's single
numeric type. `[[ -list ]]` / `[[ -record ]]` evaluate the operand's *value*
(like `-z`/`-n`), not a path stat (unlike `-f`/`-d`). A defined-but-wrong-shaped
value is false; a bare unset `$var` is an undefined-variable error (like
`-z`/`-n`), so a typo doesn't read as a silent false. Use them bare
(`[[ -list $data ]]`) — a quoted `"$data"` would test the collection's JSON
*string*, not its value.

### Crossing the boundary

A collection **displays** as compact JSON (`echo $xs` → `[10,20,30]`), but it
cannot silently cross into an OS environment variable: `export CFG=$cfg` where
`$cfg` is a list/record is a loud error — serialize it first with
`export CFG=$(tojson $cfg)`.

> Construction — list/record **literals** (`xs=[a b c]`, `{k: v}`), `push`, and
> bracket-path **assignment** (`a[b]=x`) — is designed but not yet implemented;
> today collections enter the shell via `fromjson` / `$(...)`.

## Quoting

```sh
# Double quotes - interpolation works
echo "hello $NAME"
echo "line\nbreak"              # escapes: \n \t \\ \"
echo "literal \$X"              # escaped = no interpolation

# Single quotes - literal strings, no interpolation
echo 'hello $NAME'              # prints: hello $NAME
```

### Quote to join — kaish does not paste adjacent tokens

kaish never concatenates adjacent *unquoted* tokens into one word. `$VAR`,
`$(cmd)`, and globs are each their own word; to build a single word from text
plus interpolation, **quote the whole thing**:

```sh
# Correct — one word each:
"$dir/file.txt"                # path
"out-$(date +%s).log"          # filename: text + command substitution
cat "$HOME/.config/app.toml"

# Rejected — unquoted text adjacent to an expansion is a PARSE ERROR:
echo $dir/file.txt             # error: quote the whole word "$dir/file.txt"
echo /tmp/$(id -u).sock        # error: quote "/tmp/$(id -u).sock"
```

Rather than silently splat such a word into multiple arguments, kaish rejects it
at parse time with a "quote the whole word" hint — fail-loud beats wrong argv.
Single-token words are unaffected: `file.txt`, `a.b.c`, and `v1.2.3` lex as one
token, so they need no quoting.

This is the complement of the no-word-splitting rule: kaish neither splits a
variable's value **nor** pastes neighbouring words. The "always quote
interpolated words" habit is exactly what `shellcheck --enable=all` enforces
(SC2086), so quoted kaish stays lint-clean. When in doubt, quote.

## Arguments

```sh
# Positional
echo "hello" "world"

# Flags
ls -l                           # short flag
ls -la                          # combined short flags
git commit -m "message"         # short flag with value
git push --force                # long flag
curl --header "Content-Type: json"  # long flag with value
curl --header="Content-Type: json"  # long flag with value (equals form)

# Bareword key=value — accepted only by shell-assignment builtins
# (`export FOO=bar`, `alias greet='echo hi'`, `unalias`). For every
# other command, `key=value` is a regular positional bareword:
#   cat foo=bar    # opens a file named `foo=bar`, matches bash
# Use --key value for normal flag passing.
export FOO=bar
alias greet='echo hello'
```

## Pipes & Redirects

```sh
tool-a | tool-b | tool-c        # pipe stdout → stdin
tool > file                     # redirect stdout
tool >> file                    # append stdout
tool < file                     # stdin from file
tool 2> file                    # redirect stderr
tool &> file                    # stdout + stderr
tool 2>&1                       # merge stderr into stdout
cmd 2>&1 | tee log.txt          # capture both streams

# A redirect target is a SINGLE word — quote it when it interpolates.
# Command substitution runs in the target (and in here-doc bodies).
echo hi > "$dir/out.log"        # correct
echo hi > "/tmp/$(id -u).log"   # correct — quoted text + substitution
echo hi > $(echo /tmp/x.log)    # ok — bare substitution IS the whole word
echo hi > /tmp/$(id -u).log     # parse error — bare text + substitution
cat < "$(find-config)"          # substitution in a stdin target

# Here-docs
cat <<EOF
multi-line
content here
EOF

# Here-strings — feed an expanded word (plus trailing newline) to stdin
jq -r '.name' <<< "$RESULT"     # canonical JSON field extraction
cat <<< "hello $NAME"           # interpolation works like double quotes
cat <<< 'raw $VAR'              # single quotes stay literal
```

> **One stdin source per command.** `<`, `<<`, and `<<<` all feed stdin —
> combining two of them on the same command is a parse error (rather than
> silently taking the last one, as bash does).

> **jq is built-in.** kaish ships a native jq (jaq) in-process — no external
> binary required. The `$VAR → jq <<<` idiom replaces bash's
> `echo "$VAR" | jq` without spawning a subprocess. For structured data,
> jq is the canonical access path.

### Binding kaish variables into jq

kaish's jq speaks real jq's CLI: `--arg NAME VALUE` binds a string and
`--argjson NAME VALUE` binds a JSON value. Use `-n` / `--null-input`
when you only want the bindings (no stdin).

```sh
# Stash JSON, pick a field — no subshell, no <<<
R='{"name":"amy","id":1}'
jq -n --argjson r "$R" -r '$r.name'

# Multiple bindings, in declaration order
jq -n --arg a one --arg b two -r '$a + "-" + $b'
```

Both flags are repeatable. `--argjson` errors loudly on malformed JSON
(matching real jq).

## Statement Chaining

```sh
cmd1 && cmd2                    # run cmd2 only if cmd1 succeeds
cmd1 || cmd2                    # run cmd2 only if cmd1 fails
mkdir /tmp/work && cd /tmp/work && echo "ready"
```

> **Output model:** kaish concatenates statement outputs verbatim, like bash — `printf "a"; printf "b"` and `printf "a" && printf "b"` both yield `ab`, with no separator inserted between commands. A line break appears only when a command emits its own (e.g. `echo`, which appends a trailing newline). No implicit per-statement separator is added.

## Test Expressions

```sh
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

# Shape guards — see "Collections" → "Shape guards" above
[[ -list $x ]]                  # is a native list
[[ -record $x ]]                # is a native record
# Value-typed like -z/-n (not a path stat like -f/-d). Wrong shape → false;
# a bare unset $var errors (like -z), so a typo isn't silently false.

# Comparisons
[[ $X == "value" ]]             # equality
[[ $X != "other" ]]             # inequality
[[ $NUM -gt 5 ]]                # greater than
[[ $NUM -lt 10 ]]               # less than
[[ $NUM -ge 5 ]]                # greater or equal
[[ $NUM -le 10 ]]               # less or equal
[[ $filename =~ "\.rs$" ]]      # regex match (quotes allowed, unlike bash)
[[ $input !~ "^[0-9]+$" ]]      # regex not match

# Collection membership (list → element, record → key; typed equality, so
# 443 in a list matches the JSON number, not just the string "443")
[[ banana in $fruits ]]         # element in a list
[[ name in $user ]]             # key in a record
[[ tmp not in $services ]]      # negated membership
[[ 443 in ${servers[web]} ]]    # nested path as the RHS
# A scalar/string RHS is a loud error — use =~, glob ([[ $s == *sub* ]]), or
# case for substring tests; `in` is collection-only.

# Compound expressions (with short-circuit evaluation)
[[ -f file && -d dir ]]         # logical AND
[[ -z "$VAR" || -n "$DEFAULT" ]] # logical OR
[[ ! -f /tmp/lock ]]            # logical NOT

# Precedence: ! (highest) > && > ||
[[ ! -f a || -d b && -e c ]]    # parsed as: (! -f a) || ((-d b) && (-e c))
```

Note: `[ expr ]` (single brackets) is **not** kaish syntax and there is no `test`
builtin — use `[[ ]]` for all conditionals (`[[ -f file ]] && echo yes`). This is
deliberate: `[[ ]]` is real grammar the validator checks before runtime, whereas
`test`/`[` would hide their operators as opaque runtime arguments.

## Control Flow

```sh
# Conditional
if CONDITION; then
    ...
elif OTHER_CONDITION; then
    ...
else
    ...
fi

# For loop
for ITEM in one two three; do
    echo $ITEM
done

# While loop
while CONDITION; do
    ...
done

# Case statement (patterns are glob-matched, not expanded)
case $VAR in
    hello) echo "matched hello" ;;
    *.rs) echo "Rust file" ;;
    start|run) echo "starting" ;;
    *) echo "default" ;;
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

```sh
NOW=$(date)
echo "Current time: $NOW"

RESULT=$(cat file.json | jq ".name")
```

A `$(...)` body accepts the **full statement grammar**, not just a single
pipeline: `&&`/`||` chains, `;` sequences, multi-line bodies, and `#` comments
all work. Output accumulates across the statements (no separator inserted, like
`;`), and the body's side effects (`cd`, assignments) stay contained — only the
captured stdout escapes.

```sh
HEAD=$(cd "$repo" && git rev-parse HEAD)   # && chain
BOTH=$(printf a; printf b)                 # ; sequence  → "ab"
VER=$(                                      # multi-line + comment
    grep '^version' Cargo.toml             # find the line
    | cut -d'"' -f2
)
```

Control structures (`if`/`for`/`while`/`case`) inside `$(...)` are not currently
supported — keep those at statement level.

### Structured Data and Newline Splitting in Command Substitution

Unlike traditional shells, kaish does **not** perform implicit *word* splitting on command substitution results — `$VAR` and `$(cmd)` carry whole strings, spaces and all. In `for`-loop iteration position, two narrower rules take over:

1. **Structured data wins.** Builtins that emit list-shaped output (`seq`, `find`, `glob`, `jq`, `cut`, …) carry an internal JSON array on the previous result; for-loops iterate over the array elements directly. (Pipe the data out with `kaish-last` if you need it on stdout.)
2. **Otherwise, split on newlines.** When `$(cmd)` returns a plain string with `\n` in it, the for-loop iterates per line. Whitespace within a line is never split.

```sh
# seq returns a JSON array — iterates over numbers
for i in $(seq 1 5); do
    echo "Number: $i"
done

# bare glob — expands to matching files
for f in *.rs; do
    echo "File: $f"
done

# Multi-line stdout iterates per line:
for line in $(cat hosts.txt); do echo "host: $line"; done
for hash in $(git log --format=%H | head); do git show $hash; done

# echo with no newline iterates ONCE — whitespace within a line never splits:
for x in $(echo "a b c"); do
    echo "Item: $x"            # prints "Item: a b c" once
done

# Use split for whitespace/delimiter/regex splitting:
for x in $(split "a b c"); do
    echo "Item: $x"            # prints a, b, c separately
done

# jq, cut, seq, find, glob — structured data path, one iteration per element:
for name in $(echo '["alice","bob","carol"]' | jq -r '.[]'); do
    echo "Hello $name"         # 3 iterations
done
for first in $(printf 'a,1\nb,2\n' | cut -d ',' -f 1); do
    echo "Col1: $first"        # 2 iterations
done

# Quoting suppresses the newline split — same as bash's IFS= discipline:
for x in "$(printf 'a\nb\nc')"; do
    echo "$x"                  # one iteration, newlines preserved
done
```

The newline-split rule only fires in for-loop iteration position. Assignment, argv, and string interpolation always keep `$(cmd)` whole:

```sh
RESULT=$(printf 'a\nb')                 # RESULT is "a\nb" (one string)
echo "captured: $(printf 'x\ny')"       # one echo, newline preserved inline
other_cmd $(printf 'a\nb')              # one argv slot, not two
```

**Why?** Implicit *word* splitting is a major source of shell bugs and fails `shellcheck --enable=all` (SC2086/SC2046). The newline-split-in-for rule captures bash's most common line-iteration intent — `for line in $(cat file)` — without resurrecting whitespace-splitting footguns. The "$VAR with spaces just works" promise holds everywhere; line-iteration just works in the one position where it makes sense.

## String Splitting

The `split` builtin provides explicit string splitting:

```sh
# Default: split on whitespace (like Python str.split())
for word in $(split "hello world foo"); do
    echo $word
done

# Split on delimiter
for part in $(split "a:b:c" ":"); do
    echo $part
done

# Split on regex
for part in $(split "a1b2c3" -r "[0-9]"); do
    echo $part
done

# Limit number of splits
for part in $(split "a:b:c:d" ":" --limit=2); do
    echo $part  # outputs: a, b:c:d
done
```

### Migration from Traditional Shells

If you're porting scripts that rely on word splitting:

```sh
# Old: bareword splitting of a whitespace-separated variable
for i in $ITEMS; do echo $i; done                # bash: splits on IFS
# New: explicit split — kaish flags the bareword version (E012)
for i in $(split "$ITEMS"); do echo $i; done

# Old: line iteration over command output
for line in $(cat file); do echo $line; done     # bash: splits on IFS
# New: works as-is in kaish — for-loop $(cmd) splits on newlines
for line in $(cat file); do echo $line; done
```

## Arithmetic

Arithmetic expansion is **integer-only**. Floats exist as a data type (for JSON interop) but `$(( ))` operates on integers.

```sh
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

## Shell Options

```sh
set -e                          # exit on first error
set -o latch                    # require nonce confirmation for destructive rm
set -o trash                    # move rm'd files to freedesktop.org Trash
set -o glob                     # enable bare glob expansion (on by default)
set +o latch                    # disable latch
set +o trash                    # disable trash
set +o glob                     # disable bare glob expansion
```

Options compose orthogonally: with both enabled, small files go to Trash
(no confirmation needed), large files require nonce confirmation.

Environment variables `KAISH_LATCH=1` and `KAISH_TRASH=1` enable at kernel startup.

When latch is enabled, `rm` returns **exit code 2** with a nonce:
```sh
$ rm important.dat
rm: confirmation required (latch enabled)
Authorized: important.dat
To confirm, run: rm --confirm="a3f7b2c1" important.dat
Nonce expires in 60 seconds.
```

**Latch output contract.** The prompt above is written to the result's **`err`
channel** (what a frontend renders to stderr); **stdout is empty** — nothing was
deleted, so there is no success output. The nonce is also attached as structured
data, so a program driving the shell reads it from the result rather than scraping
the stderr text. Where it lands depends on the output format:

- **No `--json`** — `ExecResult.data` is the bare nonce payload:

  ```json
  { "nonce": "a3f7b2c1", "command": "rm",
    "paths": ["important.dat"], "hint": "...", "ttl": 60 }
  ```

- **`--json`** — the result is a non-zero exit with a diagnostic, so it's wrapped
  in the standard JSON error envelope; the nonce payload is nested under `data`:

  ```json
  { "error": "rm: confirmation required (latch enabled)…",
    "code": 2,
    "data": { "nonce": "a3f7b2c1", "command": "rm", "paths": ["important.dat"],
              "hint": "...", "ttl": 60 } }
  ```

The same contract holds for the truncating-overwrite gate (`tee`, `patch`,
`sed -i`, `write`, `cp`, `mv`, `dd of=`): exit 2, human prompt on the `err`
channel, nonce on `.data` (nested under `--json`). `dd` confirms with its
`confirm=<nonce>` key=value idiom rather than `--confirm=`; copying or moving
*into* a directory (and recursive `cp -r`/`mv` of a tree) gates the named
destination, not each child file.

The `kaish-trash` builtin manages trashed files: `list`, `restore`, `empty`, `config`.

When output is truncated by the limit, the result exits **3** with `did_spill: true` and `original_code` set. The spill file path is in the output. To read it in full:

```sh
set +o output-limit
cat /run/user/1000/kaish/spill/spill-xyz.txt
set -o output-limit=8K
```

Nonces are scoped to (command, paths) — a nonce for `rm fileA` cannot confirm
`rm fileB`. They expire after 60 seconds and are not consumed on use (idempotent
retries work).

**Nonce store lifecycle:** The kernel creates a fresh `NonceStore` by default.
Frontends control persistence:
- **REPL** — one kernel per session; nonces persist across commands naturally.
- **Embedders** — pass a shared store via `KernelConfig::with_nonce_store()`
  to get cross-call persistence (e.g. a nonce issued in one `execute()` call
  confirmed in the next), or accept the default (fresh per kernel).

## Glob Expansion

Bare glob patterns in argument positions are expanded to matching files before the command runs:

```sh
ls *.txt                        # expands to all .txt files in cwd
cat src/*.rs                    # expands to .rs files in src/
for f in *.json; do             # iterates over matching files
    jq ".name" "$f"
done
```

Glob expansion is enabled by default (`set -o glob`). Disable it with `set +o glob` to pass patterns literally to tools (the pre-v0.4 behavior).

If a glob matches zero files, the command fails with exit code 1 rather than passing the literal pattern through. This prevents silent bugs where a typo in a pattern goes undetected.

The `glob` builtin still works for advanced options like `--exclude` and recursive `**` patterns:

```sh
glob "**/*.rs" --exclude="*_test.rs"
```

### Hidden files (dotfiles)

kaish follows bash's default (no `dotglob`) rule for entries whose name begins
with `.`: a leading dot is matched **only** by a pattern segment that explicitly
begins with a literal `.`. Bare wildcards never match a leading dot, and `**`
does not descend into hidden directories.

```sh
ls *                # visible entries only — .env, .github are skipped
ls .*               # the dotfiles: .env, .gitignore, …
cat .github/*       # reaches into a named dot dir (but `*` still skips .secret inside)
glob "**/.env"      # every .env, at root or under any *visible* directory
glob "**/*.rs"      # never matches .hidden.rs or files under .git/
```

The `glob` builtin's `-a`/`--hidden` flag (and any hidden-inclusive walk) acts
like `shopt -s dotglob`: bare wildcards then match dotfiles too. `find` includes
hidden entries by default.

## Error Handling

```sh
set -e                          # exit on first error

some-command || {
    echo "Command failed"
    exit 1
}

source utils.kai                # load utilities
. config.kai                    # dot notation also works
```

## Background Jobs

```sh
slow-task &                     # run in background
jobs                            # list jobs
wait                            # wait for all
wait %1 %2                      # wait for specific
```

Background jobs run in an **isolated kernel fork** (`Kernel::fork()`).
The fork snapshots the parent's session state (scope, cwd, aliases, user tools)
at the moment the job is spawned. This provides two key benefits:
1. **Full Dispatch:** Background jobs can run user-defined functions, `.kai` scripts, and command substitutions in their arguments — something traditional backgrounding often limits.
2. **Isolation:** Mutations inside a background job (e.g. `VAR=new; cd /tmp`) stay within the job's fork and do **not** leak back to the parent kernel.

## Functions

Shell-style functions using positional parameters:

```sh
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

```sh
# Create a script
write "/scripts/fetch.kai" 'echo "Fetching $1..."'

# Add to PATH
PATH="/scripts:/bin"

# Call by name (without .kai extension)
fetch "example.com"  # → Fetching example.com...
```

Scripts execute in **isolated scope** (like a subshell) — they cannot access or modify parent variables.

### External Commands

When a command is not a builtin or user-defined tool, kaish searches `PATH` for an executable:

```sh
# External commands work transparently
cargo build --release
git status
npm install

# Absolute and relative paths work directly (no PATH lookup)
/bin/echo hello                    # absolute path
./myscript.sh                      # relative path

# Virtual bin path runs builtins explicitly
/v/bin/echo hello                  # builtin via virtual path
ls /v/bin                          # list all builtins

# Pipelines work with external commands
cat file.txt | sort | uniq         # builtin | external | external
cargo test 2>&1 | grep FAILED      # external | builtin

# Format strings work (+ and - followed by non-letter are bare words)
date +%s                           # unix timestamp
date +%s%N                         # nanoseconds (%N is translated for you)
date +%Y-%m-%d                     # formatted date

# `date` follows the GNU dialect (-d, -I, -R, -r) and emits --json.
# An unknown specifier fails loud (exit 2), never panics.
date -u                            # UTC
date -I                            # ISO 8601 date (2026-06-14)
date -R                            # RFC 2822
date -d "2 days ago" +%Y-%m-%d     # relative dates
date -d "next friday"              # weekdays
date -d @1700000000                # decode an epoch (a leading @ is a bareword)
date -d "2026-06-01 -1 day"        # absolute date + offset
date --tz Asia/Tokyo               # honor an IANA timezone (also reads $TZ)
date -r file.txt                   # the file's last-modified time
date --json                        # {"iso":…,"epoch":…,"weekday":…, …}
```

**How it works:**
1. Kaish parses the command (handling quotes, variables, flags)
2. If the name contains `/`, it's used as a direct path (absolute or relative)
3. `/v/bin/name` dispatches to the builtin `name`
4. Otherwise, kaish searches `PATH` for the executable
5. Arguments are passed as a clean argv array (no shell re-parsing)
6. stdin/stdout flow correctly through pipelines

**Constraints:**
- External commands require a real filesystem working directory (not `/v/`)
- No PTY/TTY support — interactive tools like `vim` won't work
- Output is captured (not streamed) — large outputs may be truncated

**Subprocess environment (hermetic by default):**

External commands receive **only the variables kaish has marked as exported**.
The kernel does not pass through the host process's OS environment — even
`PATH` is not implicitly inherited. `export` (and the equivalent
`KernelConfig::initial_vars` on the embedder side) is the *only* way variables
reach external commands.

```sh
FOO="hello"                         # local to kaish
printenv FOO                        # → exit 1; child env has no FOO

export FOO                          # mark exported
printenv FOO                        # → "hello"
```

The bundled `kaish-repl` binary populates `initial_vars` from your shell
environment at startup, so `cargo`/`git`/`echo $PATH` work as expected
interactively. An embedder can do the same, and layer per-request `env` on top.
Embedders that want isolated execution simply leave `initial_vars` empty — see
`docs/EMBEDDING.md` for the API.

### Scope Summary

| Method | Scope | Can modify parent vars? |
|--------|-------|------------------------|
| `myfunc args` | shared | ✓ yes |
| `source script.kai` | shared | ✓ yes |
| `scriptname` (via PATH) | isolated | ✗ no |
| `cargo build` (external) | isolated | ✗ no |

## 散・集 (San/Shū) — Scatter/Gather *(experimental)*

Fan-out parallelism:

```sh
# 散 (scatter) - fan out to parallel workers
# 集 (gather) - collect results back
cat items.txt | scatter --as ITEM --limit 8 | process $ITEM | gather > results.json

# With progress
cat big_list.txt \
    | scatter --as ID --limit 4 \
    | slow-operation --id $ID \
    | gather --progress true
```

Scatter's input fans out one worker per item: structured `.data` (a JSON array
from `split`/`seq`/`glob`/`find`) spreads element-by-element, and **plain-text
stdin splits on newlines** — one worker per line, exactly like `for line in
$(cmd)` (whitespace within a line is never split; trailing newlines and `\r` are
trimmed). So `cat items.txt | scatter --as ITEM ...` runs one worker per line.

Each **scatter worker** runs in its own **parallel kernel fork**. This means
workers can run the full resolution chain: user-defined functions, `.kai`
scripts, and command substitutions in their arguments. Each worker gets its
own snapshotted scope/cwd/aliases, ensuring they don't race against each
other or the parent.

## Cancellation and Timeouts

Kaish has a single, uniform cancellation discipline that reaches every spawned external child:

- **`timeout DURATION COMMAND`** (builtin) — runs `COMMAND` with a deadline. On elapsed, the child's process group receives **SIGTERM**, then after `kill_grace` (default 2s) **SIGKILL**, then `timeout` returns exit code **124** (coreutils convention).
- **`scatter ... --timeout DUR ...`** — per-worker timeout. Hung workers are cancelled and their externals killed; the result is tagged `"timed_out": true` in `gather --format json`.
- **`Kernel::cancel()`** (embedder API) — fires the kernel's cancellation token; running externals get SIGTERM/SIGKILL via the same path. The REPL wires this to Ctrl-C.
- **`KernelConfig::request_timeout`** (embedder default) and **`ExecuteOptions::timeout`** (per-call) — apply at the kernel-call boundary; same kill behaviour, return code 124.
- **`ExecuteOptions::cancel_token`** (per-call) — embedders can pass an externally-owned `tokio_util::sync::CancellationToken` that's *raced* against the kernel's internal token. The kernel does not retain it past the call.

Cascade rules:

- **Foreground forks** (concurrent pipeline stages, scatter workers, `$(...)` cmdsubs) inherit the parent kernel's cancellation via `fork_attached`. A parent timeout/cancel kills externals running in any stage.
- **Background `&` jobs** are *detached* — they survive parent cancellation. Signal them explicitly with `kill %N`. `kill %N` (or `kill --signal TERM/KILL/INT/HUP/QUIT %N`) stops *any* job: a job that wraps external processes is signalled via their process group(s), and a pure in-process job (e.g. `sleep &`, a kaish builtin, which has no OS process group) is stopped through its cancellation token. A non-terminating signal (`--signal STOP/CONT/USR1/…`) is delivered only to jobs that spawned real external processes; sending one to a pure-builtin job is refused (there is nothing to receive it).

When `kill %N` targets external processes it signals their **process group**, so shell wrappers like `bash -c '...'` do not protect their grandchildren. SIGTERM-trapping processes are escalated to SIGKILL after `kill_grace`.

## Virtual Filesystem

VFS mounts provide unified resource access:

```
/                  → host filesystem (Passthrough/REPL) or in-memory root (Sandboxed/isolated)
/tmp/              → real /tmp (local mount)
/v/                → in-memory scratch storage
/v/bin/            → read-only builtin listing (invocable: /v/bin/echo hi)
/v/blobs/          → in-memory blob storage
/v/jobs/<id>/      → live background job state (stdout, stderr, status, command)
/dev/              → synthetic devices: /dev/null, /dev/zero, /dev/urandom, /dev/random
```

In Sandboxed and isolated (NoLocal) modes the host's real `/dev` isn't reachable,
so `/dev` is software-backed by DevFs: `/dev/null` sinks writes and reads empty,
`/dev/zero` and `/dev/urandom`/`/dev/random` are endless. The endless devices have
no whole-device read (kaish reads whole files into memory), so `cat /dev/urandom`
is a loud error — use a counted read like `head -c 32 /dev/urandom` or `dd`. In
Passthrough/REPL mode the host's real `/dev` is used.

Embedders can mount additional prefixes (e.g. `/mnt/<name>/`) via `Kernel::with_backend`.
Git is an ordinary external command (`git status`, `git log`) that runs via the
`subprocess` capability against your system `git` — there is no git builtin or VFS mount.

## Binary Data

kaish carries binary as a first-class typed value (`Value::Bytes`) that flows
through pipes intact. A bytes value **coerces to text only if it is valid UTF-8**;
otherwise an operation that requires text fails loudly rather than corrupting data.

```sh
# Produce bytes and copy them with explicit block sizing
dd if=/dev/urandom of=key.bin bs=16 count=1     # 16 random bytes to a file
dd if=/dev/zero of=/dev/null bs=1k count=10     # discard a measured stream

# Bridge text <-> bytes (no generic encode/decode — base64 and xxd cover it)
echo -n hello | base64                          # text -> base64
echo 'aGVsbG8=' | base64 -d                      # base64 -> bytes (quote padding '=')
printf '%s' hi | xxd                             # hex dump
xxd -r -p <<< 6869                               # hex -> bytes

# Inspect and measure
file key.bin                                     # identify type by magic bytes (content, not extension)
checksum key.bin                                 # hash arbitrary bytes
wc -c key.bin                                    # exact byte count
cmp a.bin b.bin                                  # byte-compare, early-exits on first diff
```

**Rendering at the boundary.** Bytes that aren't valid UTF-8 render as a hex dump
in the REPL and as a base64 envelope under `--json` (and any embedder's structured
output), so binary never garbles a terminal or a JSON channel.

**Text builtins refuse binary.** `grep`, `sed`, `awk`, `sort`, `cut`, `tr`, `jq`,
and the other text tools **error** on non-UTF-8 input instead of silently
replacing bytes with `U+FFFD`. The byte-aware movers (`cat`, `dd`, `base64`,
`xxd`, `checksum`, `wc`, `tee`, `head -c`, `tail -c`, `cmp`, `file`) consume
binary directly. External commands keep binary intact in both directions, so
`curl url > out.bin` and `... | gzip` round-trip.

## What's Intentionally Missing

These bash features are omitted because they're confusing, error-prone, or ambiguous:

| Feature | Reason | ShellCheck |
|---------|--------|------------|
| Shell brace expansion `echo {a,b,c}` | Tools support globs with braces internally | SC1083 |
| Process substitution `<(cmd)` | Use temp files | — |
| Backtick substitution `` `cmd` `` | Use `$(cmd)` — a bare backtick is a lexer error, not silently accepted | SC2006 |
| Single bracket tests `[ ]` and the `test` command | Neither exists; use `[[ ]]` for all conditionals | SC2039 |
| `eval` | Explicit is better | SC2091 |

## ShellCheck Alignment

**The Bourne-compatible subset of kaish passes `shellcheck --enable=all`.**

Features that ShellCheck warns about (word splitting, backticks) don't exist in kaish. Glob expansion is supported but controllable via `set +o glob`.

| SC Code | Warning | Kaish Approach |
|---------|---------|----------------|
| SC2006 | Use `$()` instead of backticks | Backticks don't exist — the lexer rejects them with a dedicated error |
| SC2086 | Double quote to prevent word splitting | No implicit word splitting on whitespace; `$VAR` is always one value |
| SC2046 | Quote this to prevent word splitting | `$(cmd)` is one value in argv/assignment/interp; for-loop iteration splits on newlines only |
| SC2035 | Use `./*` so globs don't expand | Bare globs expand; use `set +o glob` to disable |
| SC2039 | Use `[[ ]]` in POSIX sh | `[[ ]]` is the only test form; `[ ]` and `test` are not kaish |
| SC1083 | Escape literal braces | No shell-level brace expansion |

## Beyond Bourne

| Feature | POSIX/Bourne | Kaish | Rationale |
|---------|--------------|-------|-----------|
| **Floats** | Integer only | Native `3.14` | JSON interop |
| **Booleans** | Exit codes | Native `true`/`false` | JSON interop, clearer conditions |
| **Typed params** | None | `name:string` | Tool definitions with validation |
| **Arithmetic** | `$(( ))` | `$((expr))` with comparisons | Integer arithmetic + `>`, `<`, `==` returning 1/0 |
| **Scatter/gather** | None | `散/集` | Built-in parallelism *(experimental)* |
| **VFS** | None | `/tmp/`, `/v/` | Unified resource access |
| **Pre-validation** | None | `kaish-validate` builtin | Catch errors before execution |
| **Strict validation** | Guesses | Rejects `TRUE`, `yes`, `123abc` | Agent-friendly, fail-fast |

## Known Limitations

These are documented limitations of the current implementation:

### Preprocessing

- **Context-unaware** — The preprocessor replaces arithmetic `$(( ))` and heredoc markers before parsing. This means escape sequences or special characters inside these constructs may behave unexpectedly if they look like outer-level syntax. This is an 80/20 tradeoff for implementation simplicity.

### Validator

- **Lexical scoping** — The validator uses static analysis with ShellCheck-like strictness. It may warn about variables that would be defined at runtime if the definition isn't lexically visible. This is intentional—better to be strict than miss errors.

### Builtins

- **`set` supports `-e`, `-o latch`, `-o trash`, `-o glob`, `-o output-limit[=SIZE]`** — Unlike bash, only these options are implemented. `set -o output-limit=8K` caps command output (see Output Size Limits); `set +o output-limit` disables it. `-u`, `-x`, `pipefail` and other bash set options are silently ignored for compatibility.
- **`ps` is Linux-only** — The process listing builtin reads from `/proc` and only works on Linux systems.
- **`head`/`tail -c` counts bytes** — POSIX semantics, deliberately. A byte count can split a multi-byte UTF-8 sequence; use line-based forms (`-n`) for text.
- **`sed` is a "muscle-memory" subset, not full sed** — kaish's `sed` deliberately implements the slice of GNU/BSD (AT&T) `sed` that humans and agents actually reach for by reflex — closest in spirit to **busybox** `sed`, which is a strong influence (the supported set was [chosen from a cross-model usability panel](designing-syntax-with-llms.md)). It covers:
  - **Substitution** `s/pat/rep/[flags]` with capture groups (`\1`–`\9`, `&`) and flags `g` (global), `i`/`I` (case-insensitive), `p` (print), `m`/`M` (multiline anchors), and a numeric `N` for the Nth match (`s/x/Y/2`; combine as `Ng` for "Nth onward").
  - **Commands** `d` (delete), `p` (print), `q` (quit), `a TEXT`/`i TEXT`/`c TEXT` (append/insert/change line — the `a\TEXT`, `a TEXT`, and `aTEXT` forms all work), and `y/abc/xyz/` (transliterate).
  - **Addresses** line number `N`, last line `$`, `/regex/`, and ranges `N,M` / `/start/,/end/`.
  - **Chaining** multiple commands with `;` *or* repeated `-e` (applied in order); both forms compose into one program.
  - **Regex is ERE** (extended, like `egrep`) — *always*. `-E`/`-r` are accepted no-ops. The BRE escapes that mean something different under ERE are **rejected with a hint** instead of silently matching the wrong thing: capture groups `\(…\)` (use `(…)`), alternation `\|` (use `a|b`), and intervals `\{N,M\}` (use `a{2,5}`). A pattern-side backreference (`\1` in the pattern) is also rejected — the linear-time engine has none, in any dialect. (`\+`/`\?` are left alone: they're valid ERE escapes for literal `+`/`?`, so the BRE-vs-literal intent is ambiguous.)
  - **In-place edit** `-i` rewrites one or more file operands instead of streaming to stdout (no operands is a loud error). It routes through the latch/trash gate like `tee`/`patch`. The GNU glued backup suffix `-i.bak` is **not** supported (kaish's lexer splits it at the dot); `set -o trash` keeps a recoverable copy instead.
  - **Out of scope** (errors loudly, never half-runs): hold space (`h`/`H`/`g`/`G`/`x`), labels/branching (`b`/`t`/`:`), `w`/`r` file I/O, the `-i.bak` backup suffix, and GNU address extensions (`1~2`, `0,/re/`, `/re/,+N`). Reach for a real `sed` (external command) when you need those.

### Execution

- **Scatter results in completion order** — The 散 (scatter) construct returns results in the order jobs complete, not input order. This is inherent to parallel execution—first done, first returned.
- **Redirect targets are a single word** — Command substitution *works* in redirect targets and here-doc bodies (`cmd > $(gen-path)`, `cat < $(find-cfg)`). Because the target is one word and kaish doesn't paste adjacent tokens, quote any target that mixes literal text with an expansion: `> "/tmp/$(id -u).log"`, not the unquoted `> /tmp/$(id -u).log` (which is a parse error). See [Quoting](#quoting).

### Performance

- **Globstar `**` can be slow** — Deep directory trees with `**/*.rs` patterns may take time due to exponential matching. Consider using more specific prefixes.

### Build/Development

- **chumsky parser is pre-1.0** — The parser combinator library is the crates.io `1.0.0-alpha` series; kaish will move to stable chumsky 1.0 before its own 1.0.
- **Fuzz tests require nightly** — The fuzz testing infrastructure requires Rust nightly compiler.
