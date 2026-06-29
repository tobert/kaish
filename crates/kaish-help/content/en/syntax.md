# kaish Syntax Reference

## Variables

```bash
NAME="value"              # assignment (no spaces around =)
local NAME="value"        # local scope
COUNT=42                  # integer
PI=3.14159                # float
ENABLED=true              # boolean (only true/false)
```

## Expansion

```bash
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

## Paths

```bash
/usr/bin/foo              # absolute
../parent/file            # relative with ..
./script.sh               # dot-slash (explicit relative)
~/src/project             # tilde expands to $HOME
cd                        # bare cd goes to $HOME
cd -                      # previous directory
```

## Quoting

```bash
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

```bash
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

```bash
cmd1 && cmd2              # cmd2 if cmd1 succeeds
cmd1 || cmd2              # cmd2 if cmd1 fails
```

## Test Expressions

```bash
# File: -f (file) -d (dir) -e (exists) -r (readable) -w (writable) -x (executable)
# String: -z (empty) -n (non-empty) == != =~ (regex) !~ (not regex)
# Numeric: -gt -lt -ge -le
# Logic: && || !

[[ -f config.json && -n $NAME ]]
[[ $N -gt 5 ]]
[[ $s =~ "\.rs$" ]]
```

## Control Flow

```bash
if [[ -f file ]]; then echo "found"; elif [[ -d dir ]]; then echo "dir"; else echo "none"; fi

for item in "one" "two"; do echo $item; done
for f in *.txt; do cat "$f"; done

while [[ $N -gt 0 ]]; do N=$((N - 1)); done

case $VAR in
    hello) echo "matched" ;;
    *.rs) echo "Rust file" ;;
    *) echo "default" ;;
esac

break; continue; return [N]; exit [N]
```

## Command Substitution

```bash
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

```bash
X=$((5 + 3))              # 8
Y=$((X * 2))              # 16
# Operators: + - * / % > < >= <= == !=
# Comparisons return 1/0
```

## Functions

```bash
greet() { echo "Hello, $1!"; }
function greet { echo "Hello, $1!"; }
greet "Amy"
```

## Glob Expansion

```bash
ls *.txt                  # expands to matching .txt files
cat src/*.rs              # path-prefixed globs work
for f in *.json; do       # iterates over matches
    jq ".name" "$f"
done
set +o glob               # disable bare glob expansion
set -o glob               # re-enable (on by default)
```

Zero matches is an error (exit code 1). The `glob` builtin still works for `--exclude` and `**`.

## Shell Options

```bash
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

```bash
set -e                    # exit on first error
cmd || { echo "failed"; exit 1; }
source utils.kai          # load script (shared scope)
```

## Aliases & Background Jobs

```bash
alias ll='ls -la'         # define (first word only, not in pipelines)
unalias ll                # remove

slow-task &               # run in background
jobs                      # list jobs
wait %1 %2                # wait for specific jobs
```
