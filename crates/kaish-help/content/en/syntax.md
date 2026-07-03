# kaish Syntax Reference

## Variables

```sh
NAME="value"              # assignment (no spaces around =)
local NAME="value"        # local scope
COUNT=42                  # integer
PI=3.14159                # float
ENABLED=true              # boolean (only true/false)
```

## Expansion

```sh
$VAR                      # simple
${VAR}                    # braced
${VAR:-default}           # default if unset/empty
${#VAR}                   # string length
$0 $1 $@ $#              # script name, args, all args, count
$?                        # last exit code (0-255)
$$ ${$}                   # kaish session id — see note below
```

**`$$` is a kaish-internal session identifier**, not the OS PID. It's a
monotonic `u64` counter (starts at 1) assigned at kernel construction;
forks/subshells inherit the parent's value. This is an intentional
divergence from bash — kaish runs embedded inside long-lived host
processes, where the host PID is meaningless to the script.

## Collections (lists & records)

```sh
# CONSTRUCTION — native literal syntax, no fromjson needed. Commas optional.
xs=[apple banana cherry]  # list — space-separated, like shell words
nums=[1 2 3]               # ≡ [1, 2, 3]
u={name: amy, role: maintainer}   # record — bareword keys
compact={port:8080}        # colon may be spaced or unspaced
r={"content-type": x}      # quoted key for anything that isn't a bareword
d={"$k": x}                # double-quoted keys interpolate; '$k' stays literal
nested={tags: [a b], meta: {active: true}}   # nesting works both ways

# SPREAD (...) flattens; a bare $var nests as ONE element instead:
xs=[1 2]
ys=[0 $xs 4]                # [0,[1,2],4]  — nests
new=[...$xs date]           # [1,2,"date"] — flattens

# Values are also structured JSON — fromjson/tojson bridge real JSON text:
u=$(fromjson '{"name":"amy","tags":["rust","shell"]}')
xs=$(fromjson '[10,20,30]')

# READ ACCESS — brackets only, never dots. Bad access is a loud error.
${u[name]}                # record key (bareword = literal key)
${u[$k]}                  # dynamic key ($var)
${xs[0]}   ${xs[-1]}      # list index; negative counts from the end
${xs[0:2]}                # slice (end-exclusive) → a list
${u[tags][0]}             # nested path
${#xs}   ${#u}            # length: list elements / record keys
${u.name}                 # error — brackets only, use ${u[name]}

# ENUMERATE — always wrap the collection in $(keys ...) or $(values ...).
# A bare `for x in $xs` is an ERROR (E012): there is no word splitting.
# keys → indices (list) / keys (record).  values → elements (list) / values (record).

for x in $(values $xs); do echo $x; done          # each list element: 10 20 30
for i in $(keys $xs);   do echo $i; done          # each list index:   0 1 2
for k in $(keys $u);    do echo $k; done          # each record key:   name tags
for v in $(values $u);  do echo $v; done          # each record value

# key + value together — index the record by the loop key:
for k in $(keys $u); do echo "$k = ${u[$k]}"; done

# nested — a subscript access is still a VarRef, so it needs $() too:
for t in $(values ${u[tags]}); do echo $t; done   # rust shell

# filter while iterating — test each element, act on the matches:
for x in $(values $xs); do
  if [[ $x -gt 15 ]]; then echo "big: $x"; fi      # big: 20  big: 30
done

# MEMBERSHIP — RHS must be a collection (see Test Expressions):
if [[ rust in $(values ${u[tags]}) ]]; then echo "has it"; fi   # element present?
if [[ name in $u ]]; then echo "has it"; fi                     # record has key?
if [[ 1 in $(keys $xs) ]]; then echo "in bounds"; fi            # index in bounds?

# SHAPE GUARD — an API sometimes returns a list, sometimes a record; check
# before committing to keys/values/for. typeof + [[ -list ]] / [[ -record ]]:
if [[ -record $data ]]; then
  for k in $(keys $data); do echo $k; done
elif [[ -list $data ]]; then
  for x in $(values $data); do echo $x; done
fi

tojson $u                 # serialize back to JSON text (--pretty to indent)

# ASSIGNMENT — the same bracket paths write, in place, no spaces around `=`.
# No autovivification — every intermediate must already exist with the right
# shape; the ONLY thing a path-set may create is the final record key.
xs[0]=9                   # in-bounds index update (negative index works too)
u[host]=localhost         # record key: insert or update
s[web][port]=9000         # deep path
xs[9]=x                   # error — index out of bounds (push grows lists)
s[api][port]=1            # error — no `api` key (no autoviv; init it first)
user.email=x              # error — brackets only, use `user[email]=x`

# push appends to a top-level LIST in place — takes the NAME (like read/unset)
push xs date               # xs is now [...,"date"]
push xs $rec               # values push as typed Values, not stringified text

# KNOWN LIMITATION — push only takes a top-level bareword target; a bracket
# path target fuses into a glob and fails loudly ("no matches") instead of
# pushing. Read the nested list out, push, assign it back:
push services[web][tags] item   # ERROR — no matches: services[web][tags]
tmp=${services[web][tags]}      # workaround
push tmp item
services[web][tags]=$tmp
```

## Paths

```sh
/usr/bin/foo              # absolute
../parent/file            # relative with ..
./script.sh               # dot-slash (explicit relative)
~/src/project             # tilde expands to $HOME
cd                        # bare cd goes to $HOME
cd -                      # previous directory
```

## Quoting

```sh
"hello $NAME"             # double quotes — interpolation
"literal \$X"             # escape $ to prevent expansion
'hello $NAME'             # single quotes — literal, no interpolation

# Quote to JOIN text with interpolation — kaish does not paste adjacent
# unquoted tokens into one word (no implicit concatenation):
"$dir/file.txt"           # one path
"out-$(date +%s).log"     # one filename (text + command substitution)
echo "/tmp/$(id -u).sock" # one argument

# Unquoted text adjacent to an expansion is a PARSE ERROR (quote the word):
echo $dir/file.txt        # error — quote "$dir/file.txt"
echo /tmp/$(id -u).sock   # error — quote "/tmp/$(id -u).sock"
cmd > $dir/out.txt        # error — quote "$dir/out.txt"
# (single-token words like file.txt or v1.2.3 are fine unquoted)
```

## Pipes & Redirects

```sh
cmd1 | cmd2 | cmd3        # pipe stdout
cmd > file                # write stdout
cmd >> file               # append
cmd < file                # stdin from file
cmd 2> file               # stderr
cmd &> file               # stdout + stderr
cmd 2>&1                  # merge stderr into stdout

cat <<EOF                 # here-doc
content with $VAR
EOF

jq -r '.name' <<< "$R"    # here-string — feed expanded word to stdin

cmd > "$dir/out.log"      # quote interpolated targets — one word required
cat < "$(find-config)"    # command substitution works in a quoted target
```

A redirect target is a single word: quote it when it interpolates
(`> "$dir/f"`, not `> $dir/f`). Bare command substitution as the whole
target (`> $(cmd)`) works; bare text-plus-interpolation does not.

One stdin source per command: `<`, `<<`, and `<<<` cannot be combined.
jq is built-in (native jaq), so `<<<` + jq replaces `echo … | jq`
without a subprocess. jq also accepts real jq's `--arg NAME VALUE`,
`--argjson NAME VALUE`, and `-n` / `--null-input` flags for binding
kaish variables directly into the filter.

## Operators

```sh
cmd1 && cmd2              # cmd2 if cmd1 succeeds
cmd1 || cmd2              # cmd2 if cmd1 fails
```

## Test Expressions

```sh
# File: -f (file) -d (dir) -e (exists) -r (readable) -w (writable) -x (executable)
# String: -z (empty) -n (non-empty) == != =~ (regex) !~ (not regex)
# Shape guard: -list -record — the value's shape, not a path stat. A
#   defined-but-wrong-shaped value is false; a bare unset $var errors (like
#   -z), so a typo isn't silently false. Pairs with the typeof builtin.
# Numeric: -gt -lt -ge -le
# Logic: && || !
# Membership: in (list→element, record→key) / not in — RHS must be a collection

[[ -f config.json && -n $NAME ]]
[[ $N -gt 5 ]]
[[ $s =~ "\.rs$" ]]
if [[ banana in $fruits ]]; then echo "have one"; fi
if [[ tmp not in $services ]]; then echo "not running"; fi
[[ -list $x ]]
[[ -record $x ]]
```

## Control Flow

```sh
if [[ -f file ]]; then echo "found"; elif [[ -d dir ]]; then echo "dir"; else echo "none"; fi

for item in "one" "two"; do echo $item; done
for f in *.txt; do cat "$f"; done
for x in $(values $list); do echo $x; done            # a collection's elements/values
for k in $(keys $rec); do echo "$k=${rec[$k]}"; done  # a record's keys (bare $rec is E012)

while [[ $N -gt 0 ]]; do N=$((N - 1)); done

case $VAR in
    hello) echo "matched" ;;
    *.rs) echo "Rust file" ;;
    *) echo "default" ;;
esac

break; continue; return [N]; exit [N]
```

## Command Substitution

```sh
NOW=$(date)

# In for-loops, $(cmd) splits on newlines (only):
for line in $(cat file); do echo $line; done   # per-line iteration
for x in $(echo "a b c"); do echo $x; done     # one iteration (no \n)

# Whitespace splitting needs explicit split:
for x in $(split "a b c"); do echo $x; done

# Builtins that emit .data (seq, jq, cut, find, glob) iterate per element:
for i in $(seq 1 5); do echo $i; done

# Outside for-loops, $(cmd) is one value:
R=$(printf 'a\nb')                # R is "a\nb"
echo "got: $(printf 'x\ny')"      # one echo, newline preserved

# A $(...) body takes the full statement grammar: &&/|| chains, ; sequences,
# multi-line bodies, and # comments (not just a single pipeline):
H=$(cd "$repo" && git rev-parse HEAD)
B=$(printf a; printf b)           # "ab"

# kaish-last prints the previous command's .data (or its stdout) as text:
seq 1 5
kaish-last | jq '.[2]'            # → 3
seq 1 5
DATA=$(kaish-last)                # capture for later use
```

## Arithmetic

```sh
X=$((5 + 3))              # 8
Y=$((X * 2))              # 16
# Operators: + - * / % > < >= <= == !=
# Comparisons return 1/0
```

## Functions

```sh
greet() { echo "Hello, $1!"; }
function greet { echo "Hello, $1!"; }
greet "Amy"
```

## Glob Expansion

```sh
ls *.txt                  # expands to matching .txt files
cat src/*.rs              # path-prefixed globs work
for f in *.json; do       # iterates over matches
    jq ".name" "$f"
done
set +o glob               # disable bare glob expansion
set -o glob               # re-enable (on by default)
```

Zero matches is an error (exit code 1). The `glob` builtin still works for `--exclude` and `**`.

## Regex (grep, sed, awk)

```sh
# ERE (egrep-style) everywhere: a|b  (…)  x+ y? z{2,5}  [a-z]  ^…$
grep 'error|warn' log.txt           # alternation — one call, many terms
sed -E 's/v([0-9]+)\.[0-9]+/\1/'    # (…) capture; \1 \2 in the REPLACEMENT
awk '/^(GET|POST) /' access.log     # same engine in all three

# GNU BRE spellings are accepted too — \| \(…\) \{n,m\} \+ \? rewrite to ERE:
grep 'error\|warn' log.txt          # ≡ error|warn
sed 's/\(a\)\(b\)/\2\1/'            # ≡ s/(a)(b)/\2\1/

# A literal | + ? ( ) { }: bracket class, or strict-ERE mode
grep '[|]' f                        # literal pipe (works in all three)
grep -E 'a\|b' f                    # -E / -r (grep, sed): backslashed meta = literal
grep -F 'a|b' f                     # -F: fixed string, nothing is a metachar

grep '\d+\.\w\b' f                  # \d \w \s \b \. work; \< \> too
sed 's/(a)\1/x/'                    # ERROR — no backreference IN a pattern
```

## Shell Options

```sh
set -e                    # exit on first error
set -o latch              # require nonce confirmation to delete/overwrite (exit code 2)
set -o trash              # move rm'd / overwritten files to Trash
set -o glob               # enable bare glob expansion (on by default)
set +o latch              # disable latch
set +o trash              # disable trash
set +o glob               # disable bare glob expansion
```

Env vars: `KAISH_LATCH=1`, `KAISH_TRASH=1` enable at startup.

**Latch:** Nonces scoped to (command, paths). A nonce for `rm A` rejects `rm B`.
Confirmed paths must be subset of authorized paths. Exit code 2 = needs confirmation.
Applies to `rm` and to truncating overwrites (`tee`, `patch`, `sed -i`) —
confirm those with `--confirm=<nonce>`. `tee -a` append, new files, and
`patch --dry-run` don't gate. The prompt prints to stderr (stdout stays empty);
the nonce is on the result's `data` field — nested under `data` in the error
envelope when `--json` is on.

**Trash:** Files <= 10MB and directories always trash. `/tmp`, `/v/*` excluded.
A truncating overwrite under `trash` snapshots the file's prior content first
(recoverable from Trash). If trash fails, the op errors (no silent fallthrough to a
destructive delete/overwrite). Configure threshold: `kaish-trash config max-size <bytes>`.

**Nonce persistence:** The kernel creates a fresh nonce store by default.
An embedder can share one store across `execute()` calls in a session, so a
nonce from call 1 can confirm in call 2. The REPL keeps one kernel alive —
nonces persist naturally. Embedders control this via
`KernelConfig::with_nonce_store()`.

## Error Handling

```sh
set -e                    # exit on first error
cmd || { echo "failed"; exit 1; }
source utils.kai          # load script (shared scope)
```

## Aliases & Background Jobs

```sh
alias ll='ls -la'         # define (first word only, not in pipelines)
unalias ll                # remove

slow-task &               # run in background
jobs                      # list jobs
wait %1 %2                # wait for specific jobs
```
