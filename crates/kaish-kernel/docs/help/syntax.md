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
```

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
```

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
for f in $(glob "*.txt"); do cat "$f"; done

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
# No implicit word splitting — $(echo "a b c") iterates once
# Use split for explicit splitting:
for x in $(split "a b c"); do echo $x; done
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

## Shell Options

```bash
set -e                    # exit on first error
set -o latch              # require nonce confirmation for rm (exit code 2)
set -o trash              # move rm'd files to Trash
set +o latch              # disable latch
set +o trash              # disable trash
```

Env vars: `KAISH_LATCH=1`, `KAISH_TRASH=1` enable at startup.

**Latch:** Nonces scoped to (command, paths). A nonce for `rm A` rejects `rm B`.
Confirmed paths must be subset of authorized paths. Exit code 2 = needs confirmation.

**Trash:** Files <= 10MB and directories always trash. `/tmp`, `/v/*` excluded.
If trash fails, rm errors (no silent fallthrough to permanent delete).
Configure threshold: `kaish-trash config max-size <bytes>`.

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
