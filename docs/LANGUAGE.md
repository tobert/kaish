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

### Construction — list/record literals

Lists and records also have native syntax, no `fromjson` needed. Commas are
optional in both — whitespace, commas, or a mix all work as separators:

```sh
xs=[apple banana cherry]     # list — space-separated, like shell words
nums=[1 2 3] ≡ [1, 2, 3]     # commas optional
empty=[]
dog=[dog]                    # single-element list (glued `[dog]` still a list, never a glob)

user={name: amy, role: maintainer}     # record — string-keyed map
compact={port:8080}                    # colon may be spaced or unspaced — same value
r={"content-type": x}                  # quoted key for anything that isn't a bareword
d={"$k": x}                            # double-quoted keys interpolate ($k resolves);
                                       # single quotes keep a literal $ ({'$k': x})

nested={tags: [a b], meta: {active: true}}   # nesting works both ways
```

Multi-line literals consume interior newlines instead of ending the statement,
so a trailing comma before the closing brace is fine:

```sh
services={
  web: {port: 8080, replicas: 3},
  api: {port: 9000, replicas: 2},
}
```

A list element or record value is exactly **one** word or **one** quoted
string — `["green apple" banana]`, `{msg: "hello world"}`. An unquoted
multi-word value (`{msg: hello world}`) is a parse error with the quoted fix
in the message, never silently split or joined.

**Spread (`...`) flattens; a bare variable nests.** Inside `[ ]`, a bare
`$var` is ONE element (it nests); `...$var` flattens that list's elements into
the new one:

```sh
xs=[1 2]
ys=[0 $xs 4]        # [0,[1,2],4]     — nests (one element)
new=[...$xs date]   # [1,2,"date"]    — flattens
both=[...$a ...$b]  # concatenates two lists
```

Collection literals are **value-position only** — an assignment RHS, the
`in`/`not in` right-hand operand, or nested inside another literal. They never
appear in a command argument (`ls [dog]` is still a glob) or a `for`-head item
(`for x in [a]` is still a word list) — see "Iteration is `$()`-only" below.
Assigning a `[`-leading glob (`logs=[0-9]*.log`) is a parse error at value
position; use `$(glob '[0-9]*.log')` if you actually want glob expansion there.

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
echo ${X:-${Y:-fallback}}     # defaults nest — first set value wins
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
if [[ rust in $(values ${u[tags]}) ]]; then echo "has it"; fi   # element present in a list?
if [[ name in $u ]]; then echo "has it"; fi                     # record has key?
if [[ 1 in $(keys $xs) ]]; then echo "in bounds"; fi            # index in bounds?
if [[ tmp not in $u ]]; then echo "absent"; fi                  # absent?
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

### Assignment — bracket-path lvalues + `push`

The same bracket paths that read also write, in place, with **no
spaces around `=`**:

```sh
xs=[1 2 3]
xs[0]=9                     # in-bounds index update
xs[-1]=7                    # negative index works too

u={port: 8080}
u[host]=localhost           # inserts a new key
u[port]=9090                # updates an existing key

s={web: {port: 8080}}
s[web][port]=9000           # deep path
```

**No autovivification** — every intermediate segment must already exist with
the right shape; the *only* thing a path-set may create is the **final**
record key. An out-of-bounds list index, a missing intermediate key, a
scalar or undefined root, and a slice lvalue (`xs[0:2]=x`) are all loud
errors — `push` is how lists grow, and `x={}` / `x=[]` initializes an
intermediate before you subscript into it:

```sh
xs=[1 2 3]; xs[9]=x                        # error: index out of bounds
s={web: {port: 8080}}; s[api][port]=1      # error: no such key (no autoviv)
y=hi; y[0]=x                                # error: not a collection
z[0]=x                                      # error: z is not defined
```

A dotted assignment target is also rejected — kaish is brackets-only, and the
`Ident` token otherwise admits `.` for other uses (filenames):

```sh
user.email=x     # error — use `user[email]=x`
```

`push` appends to a **list** variable in place — a top-level name or a
bracket path. Like `read`/`unset`, it takes the variable **name** (bareword),
not `$name` — this is what lets it write back to the caller:

```sh
xs=[a b]
push xs c                          # xs is now [a b c] — mutated in place
push xs $rec                       # values push as typed Values, not stringified text
echo ${#xs} ${xs[-1]}              # 3 c

services={web: {tags: []}}
push services[web][tags] canary    # bracket-path target — same lvalue rules as an assignment
echo ${services[web][tags]}        # ["canary"]
```

`push` to an undefined or non-list target is a loud error, never a silent
create. A bracket-path target follows the same lvalue rules as an
assignment — no autovivification: every intermediate segment must already
exist, and the resolved leaf must be a list.

### Crossing the boundary

A collection **displays** as compact JSON (`echo $xs` → `[10,20,30]`), but it
cannot silently cross into an OS environment variable: `export CFG=$cfg` where
`$cfg` is a list/record is a loud error — serialize it first with
`export CFG=$(tojson $cfg)`. Env-prefix assignment (`FOO=bar cmd`) stays
bare-ident-only for the same reason — a subscripted target there
(`user[email]=x cmd`) is never captured as an exported, command-scoped
variable.

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

### The text boundary — one document vs. many (JSONL)

`fromjson` / `tojson` and kaish's `jq` all take **exactly one JSON document**
of text — trailing data after it is a loud error, not a silent slurp. A
stream of *many* documents (NDJSON/JSONL: an API response, a log file, kaish's
own `gather` output) needs an explicit door:

```sh
curl -s api/events.ndjson | fromjsonl | scatter -- restart ${ITEM[host]}
cat results.jsonl | fromjsonl | jq '.[] | select(.ok | not)'   # ingress: text → typed list
xs=$(fromjson '[1,2,3]'); tojsonl $xs                          # egress: list → JSONL text
```

`fromjsonl` is strictly line-oriented (one document per line, blank lines
skipped, any bad line — including a truncated trailing one — is a loud error
naming the line number); a pretty-printed multi-line document is a loud error
pointing at `fromjson` (one document) or `jq -s` (below). `tojsonl` is the
egress mirror: a list in, one compact document per line out — a record or
scalar is one document, so that's `tojson`'s job instead.

`jq -s` / `--slurp` also reads a document stream, with real jq framing (so
pretty multi-line documents are fine) and *always* wraps the result in an
array — even a single document (`printf '{"a":1}' | jq -s length` → `1`, the
array length). On the `.data` pipeline path the upstream stage has already
handed over one structured value — that's the single "document" — so `-s`
wraps *that* value in a one-element array, same as the text path
(`fromjson '{"a":1}' | jq -s length` → `1`; `... | jq -s '.[0]'` → the
original value). If plain `jq` (no `-s`) sees JSONL-shaped input, the error
names the document count and points at `fromjsonl` or `jq -s` instead of a
bare parse failure.

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
if [[ banana in $fruits ]]; then echo "have one"; fi          # element in a list
if [[ name in $user ]]; then echo "known"; fi                 # key in a record
if [[ tmp not in $services ]]; then echo "not running"; fi    # negated membership
if [[ 443 in ${servers[web]} ]]; then echo "listening"; fi    # nested path as the RHS
# A scalar/string RHS is a loud error — use =~, glob ([[ $s == *sub* ]]), or
# case for substring tests; `in` is collection-only.

# Compound expressions (with short-circuit evaluation)
[[ -f file && -d dir ]]         # logical AND
[[ -z "$VAR" || -n "$DEFAULT" ]] # logical OR
[[ ! -f /tmp/lock ]]            # logical NOT

# Precedence: ! (highest) > && > ||
[[ ! -f a || -d b && -e c ]]    # parsed as: (! -f a) || ((-d b) && (-e c))
```

### The `test` command

`test EXPR` is the command form of the same conditionals — exit `0` if true, `1`
if false, `2` on a usage or type error. It is a real builtin (VFS-aware,
validated), not a shell-out to the host `/usr/bin/test`:

```sh
test -f config.toml             # file tests: -e -f -d -r -w -x
test -z "$out"                  # string tests: -z -n
test "$mode" = release          # equality: = == != (literal, not glob)
test "$count" -gt 0             # numeric: -eq -ne -gt -lt -ge -le
test ! -d build                 # negation: a single leading !
test -f a && test -f b          # compound: chain with shell && / ||
if test -f "$path"; then …; fi  # the usual home, an `if`/`while` condition
```

`test` follows `[[`'s semantics, with a few deliberate, predictable differences:

- **Numbers are kaish (JSON) numbers**, so `test 1.5 -gt 1` compares (it does not
  error the way POSIX `test` would); a non-numeric operand is a loud error.
- **No `-a` / `-o` / `\( \)`** — those ambiguous XSI operators are rejected
  loudly. Compose with shell `&&` / `||`, or use `[[ ]]`.
- **An operator missing its operand is a loud error** (`test -f` on its own),
  never a surprise-true — kaish does no word splitting, so the classic
  `test -n $empty` footgun can't arise.
- **Richer tests stay `[[ ]]`-only**: membership (`in` / `not in`), regex
  (`=~` / `!~`), and the shape guards (`-list` / `-record`).

Prefer `[[ ]]` — it is real grammar the validator checks before runtime, carries
the richer tests, and expresses compound logic (`&&` / `||` / `!`) in one
construct. Reach for `test` for muscle memory or where a plain command is wanted
(a condition builtin, a `&&` chain). `[ expr ]` (single brackets) remains **not**
kaish syntax — `[` opens a list literal, so use `test` or `[[ ]]`.

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
deleted, so there is no success output. The nonce is also attached as a
first-class typed field, so a program driving the shell reads it from the result
rather than scraping the stderr text. It is control-plane, distinct from the
data-plane `.data`:

- **No `--json`** — `ExecResult.latch` is the typed request
  `Some(LatchRequest { nonce, command, paths, hint, ttl })` (embedders read it
  via `latch_request()`).

- **`--json`** — the result is a non-zero exit with a diagnostic, so it's wrapped
  in the standard JSON error envelope; the request is surfaced under its own
  `latch` key (never folded into `data`):

  ```json
  { "error": "rm: confirmation required (latch enabled)…",
    "code": 2,
    "latch": { "nonce": "a3f7b2c1", "command": "rm", "paths": ["important.dat"],
               "hint": "...", "ttl": 60 } }
  ```

The same contract holds for the truncating-overwrite gate (`tee`, `patch`,
`sed -i`, `write`, `cp`, `mv`, `dd of=`): exit 2, human prompt on the `err`
channel, request on `.latch` (under the `latch` key with `--json`). `dd` confirms with its
`confirm=<nonce>` key=value idiom rather than `--confirm=`; copying or moving
*into* a directory (and recursive `cp -r`/`mv` of a tree) gates the named
destination, not each child file.

Files under 10MB and all directories go to trash (configurable via
`kaish-trash config max-size`); larger files are deleted permanently. Excluded
paths (`/tmp`, `/v/*`) bypass trash. If the trash operation fails, `rm` returns
an error — it never silently falls through to permanent delete.

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

The `glob` builtin is the exception: it consumes patterns as data, so the
binder hands them through as written — no quoting needed, and it accepts
multiple patterns. Use it for advanced options like `--exclude` and recursive
`**` patterns:

```sh
glob **/*.rs --exclude="*_test.rs"   # pattern reaches glob unexpanded
glob **/*.rs **/*.toml               # multiple patterns, deduped union
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

### Ignore-aware filtering (`.gitignore`, `kaish-ignore`)

The reference REPL is ignore-aware by default: interactive, `-c`, and script
modes load `.gitignore` plus a default ignore list (`.git`, `node_modules`,
`target`, `__pycache__`, `.venv`, `venv`, `dist`, `build`, `.next`) at
**Advisory** scope. "Polite" file-walking tools (`glob`, `tree`, `grep -r`,
`ls -R`) filter these out; `find` stays POSIX-unrestricted and always sees
everything. A bare embedded kernel (`transient`/`named`/`isolated`) keeps the
unfiltered default — only the reference REPL opts in.

```sh
glob '**/*.rs'                  # skips target/, .git/, etc. by default
glob '**/*.rs' --no-ignore      # this call only: no filtering
kaish-ignore clear              # this session: no filtering at all
kaish-ignore                    # show current config
```

See `help ignore` for the full `kaish-ignore` builtin (`add`/`remove`/`clear`/
`defaults`/`auto`/`scope`) and the Agent-mode Enforced scope, where `find`
filters too.

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

## 散・集 (San/Shū) — Scatter/Gather

The kaish-native **typed parallel map** (GH #73; panel-validated 2026-07-03).
A future `xargs` builtin will own the POSIX bare-lines flavor; scatter/gather
is unapologetically structured.

```sh
# 散 (scatter) fans structured input out to workers; 集 (gather) collects
# one JSONL result record per worker, in item order, FAILURES INCLUDED:
cat hosts.txt | scatter --as H --limit 8 | ping -c1 "$H" | gather
#   {"i":0,"item":"web1","ok":true,"code":0,"out":"…","err":""}
#   {"i":1,"item":"db1","ok":false,"code":1,"out":"","err":"…"}

# ITEM is TYPED — a JSON-array element keeps its real type:
jq '.jobs' jobs.json | scatter --limit 4 \
    | deploy --id "${ITEM[id]}" --host "${ITEM[host]}" | gather

# Consume: kaish jq receives the records as ONE ARRAY — stream with .[] :
… | gather | jq -r '.[] | select(.ok) | .out'
# or iterate typed records:
for r in $(… | gather); do
  if [[ ${r[ok]} == false ]]; then retry "${r[item]}"; fi
done

… | gather --json     # one JSON array instead of JSONL
… | gather --lines    # raw successful outputs; HARD ERROR if any worker failed
```

- **Row schema**: `i`, `item` (typed), `ok`, `code`, `out` (stdout, trailing
  newline stripped), `err` (stderr, always present) on every row; `data` (the
  worker's structured output) and `timed_out:true` when present. Timeout →
  `code` 124.
- **Exit codes**: `0` all workers ok · `123` any worker failed (partial or
  total — the rows carry which) · `2` usage.
- **Ingress**: a JSON array fans out typed, element-by-element (`1` and `"1"`
  stay distinct); plain text is one string item per line (blank lines skipped,
  whitespace within a line never split). A single non-array object errors
  loudly with a select-the-array hint; `null` items and binary input error
  loudly. Empty input exits 0 with no rows.
- Quote `"$ITEM"` at command boundaries — a bare record at an external
  command's argv is the usual loud boundary error.

Each **scatter worker** runs in its own **parallel kernel fork**. This means
workers can run the full resolution chain: user-defined functions, `.kai`
scripts, and command substitutions in their arguments. Each worker gets its
own snapshotted scope/cwd/aliases, ensuring they don't race against each
other or the parent. A worker's `code` is its pipeline segment's last
command's exit code; all workers run to completion (gather never
short-circuits the rest on a failure).

## Cancellation and Timeouts

Kaish has a single, uniform cancellation discipline that reaches every spawned external child:

- **`timeout DURATION COMMAND`** (builtin) — runs `COMMAND` with a deadline. On elapsed, the child's process group receives **SIGTERM**, then after `kill_grace` (default 2s) **SIGKILL**, then `timeout` returns exit code **124** (coreutils convention).
- **`scatter ... --timeout DUR ...`** — per-worker timeout. Hung workers are cancelled and their externals killed; the result row is tagged `"timed_out": true` with `code` 124.
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
/v/jobs/<id>/      → live background job state (stdout, stderr, status, command, latch)
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
`xxd`, `checksum`, `tee`, `head -c`, `tail -c`, `cmp`, `file`) consume binary
directly. `wc -c`/`wc -l` are pure byte-level counts (exact length, raw `\n`
scan) and consume binary directly too; `wc -m`/`-w`/default need a text view
and **error** on non-UTF-8 input like the other text tools rather than
over-counting via `U+FFFD`. External commands keep binary intact in both
directions, so `curl url > out.bin` and `... | gzip` round-trip. A `< file`
redirect (or an embedder's `ExecuteOptions::with_stdin`) forwards binary intact
too — only the command actually reading it decides whether that's fine.

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

### Lexing

- **Arithmetic doesn't paste onto words** — `echo $((1+2))abc` is a loud parse
  error with a quoting hint (bash prints `3abc`; kaish never pastes adjacent
  tokens). Quote the whole word instead: `echo "$((1+2))abc"` prints `3abc`.
- **No arithmetic inside a bare `${...}`** — `${X:-$((1+2))}` is a loud error
  (`ArithmeticInVarRef`); assign the expression to a variable first
  (`N=$((expr)); ${X:-$N}`). Inside a double-quoted string the same construct
  works via string interpolation.

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
  - **Regex is ERE** (extended, like `egrep`) by default, with a **GNU BRE-superset**: the BRE backslash-metas `\|` (alternation), `\(…\)` (groups), `\{N,M\}` (intervals), and `\+`/`\?` (quantifiers) are rewritten to their ERE meaning, so agent-idiomatic `s/cat\|dog/X/` and `s/\(a\)\(b\)/\2\1/` work instead of silently matching a literal `|`/`(`. Pass **`-E`/`-r`** for strict ERE, where those escapes match the literal character — the escape hatch for a literal `|`/`+`. The narrow trade-off: in the default dialect a backslashed meta is the operator, never a literal (match the character with a bracket class, `[|]`/`[+]`). A pattern-side backreference (`\1` in the pattern) is still rejected — the linear-time engine has none, in any dialect. (`grep` and `awk` share this superset; `awk` has no `-E` flag, so it always rewrites.)
  - **In-place edit** `-i` rewrites one or more file operands instead of streaming to stdout (no operands is a loud error). It routes through the latch/trash gate like `tee`/`patch`. The GNU glued backup suffix `-i.bak` is **not** supported (kaish's lexer splits it at the dot); `set -o trash` keeps a recoverable copy instead.
  - **Out of scope** (errors loudly, never half-runs): hold space (`h`/`H`/`g`/`G`/`x`), labels/branching (`b`/`t`/`:`), `w`/`r` file I/O, the `-i.bak` backup suffix, and GNU address extensions (`1~2`, `0,/re/`, `/re/,+N`). Reach for a real `sed` (external command) when you need those.

### Execution

- **Scatter results in completion order** — The 散 (scatter) construct returns results in the order jobs complete, not input order. This is inherent to parallel execution—first done, first returned.
- **Redirect targets are a single word** — Command substitution *works* in redirect targets and here-doc bodies (`cmd > $(gen-path)`, `cat < $(find-cfg)`). Because the target is one word and kaish doesn't paste adjacent tokens, quote any target that mixes literal text with an expansion: `> "/tmp/$(id -u).log"`, not the unquoted `> /tmp/$(id -u).log` (which is a parse error). See [Quoting](#quoting).
- **Recursion is depth-guarded** — Command substitution (`$(...)`), shell-function calls, `.kai` script execution, and `source`/`.` all re-enter the interpreter on the native stack; a runaway or mutually recursive script (`f() { f; }; f`) hits a cap (`MAX_RECURSION_DEPTH`, 48 levels) and fails loudly with `maximum recursion depth exceeded` instead of overflowing the stack. See `docs/EMBEDDING.md` for the embedder-side stack-sizing contract.

### Performance

- **Globstar `**` can be slow** — Deep directory trees with `**/*.rs` patterns may take time due to exponential matching. Consider using more specific prefixes.

### Build/Development

- **Fuzz tests require nightly** — The fuzz testing infrastructure requires Rust nightly compiler.
