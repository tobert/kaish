# kaish Syntax Reference

## Variables

```bash
# Assignment (no spaces around =)
NAME="value"
local NAME="value"    # local scope

# Types
COUNT=42              # integer
PI=3.14159            # float
ENABLED=true          # boolean (only true/false, not yes/1/TRUE)
DATA='{"key":"val"}'  # JSON as string
```

## Expansion

```bash
$VAR                  # simple expansion
${VAR}                # braced expansion
${VAR:-default}       # default if unset/empty
${#VAR}               # string length

# Positional
$0                    # script/function name
$1 $2 $3              # arguments
$@                    # all arguments
$#                    # argument count
$?                    # last exit code (0-255)
```

## Paths

```bash
/usr/bin/foo          # absolute path
../parent/file        # relative path with ..
./script.sh           # dot-slash (explicit relative)
~/src/project         # tilde expands to $HOME
~                     # bare tilde = $HOME
..                    # parent directory
cd                    # bare cd goes to $HOME
cd -                  # previous directory
```

## Quoting

```bash
# Double quotes — interpolation works
echo "hello $NAME"
echo "line\nbreak"    # escapes: \n \t \\ \"
echo "literal \$X"    # escape $ to prevent expansion

# Single quotes — literal, no interpolation
echo 'hello $NAME'    # prints: hello $NAME
```

## Pipes & Redirects

```bash
cmd1 | cmd2 | cmd3    # pipe stdout
cmd > file            # write stdout
cmd >> file           # append stdout
cmd < file            # stdin from file
cmd 2> file           # stderr
cmd &> file           # stdout + stderr
cmd 2>&1              # merge stderr into stdout

# Here-docs
cat <<EOF
multi-line
content
EOF
```

## Statement Chaining

```bash
cmd1 && cmd2          # cmd2 only if cmd1 succeeds
cmd1 || cmd2          # cmd2 only if cmd1 fails
```

## Test Expressions

```bash
# File tests
[[ -f path ]]         # is file
[[ -d path ]]         # is directory
[[ -e path ]]         # exists
[[ -r path ]]         # readable
[[ -w path ]]         # writable
[[ -x path ]]         # executable

# String tests
[[ -z $VAR ]]         # empty
[[ -n $VAR ]]         # non-empty

# Comparisons
[[ $X == "value" ]]   # equal
[[ $X != "other" ]]   # not equal
[[ $N -gt 5 ]]        # greater than
[[ $N -lt 10 ]]       # less than
[[ $N -ge 5 ]]        # greater or equal
[[ $N -le 10 ]]       # less or equal
[[ $s =~ "pat" ]]     # regex match
[[ $s !~ "pat" ]]     # regex not match

# Compound
[[ A && B ]]          # logical AND
[[ A || B ]]          # logical OR
[[ ! A ]]             # logical NOT
```

## Control Flow

```bash
# Conditional
if CONDITION; then
    ...
elif OTHER; then
    ...
else
    ...
fi

# For loop
for ITEM in "one" "two" "three"; do
    echo $ITEM
done

# Iterate command output
for f in $(glob "*.txt"); do
    cat "$f"
done

# While loop
while CONDITION; do
    ...
done

# Case
case $VAR in
    hello) echo "matched" ;;
    *.rs) echo "Rust file" ;;
    y|yes) echo "yes" ;;
    *) echo "default" ;;
esac

# Control statements
break                 # exit loop
continue              # next iteration
return [N]            # return from function
exit [N]              # exit script
```

## Command Substitution

```bash
NOW=$(date)
RESULT=$(cat file.json | jq ".name")

# No implicit word splitting!
for x in $(echo "a b c"); do
    echo $x           # prints "a b c" once
done

# Use split for explicit splitting
for x in $(split "a b c"); do
    echo $x           # prints a, b, c separately
done
```

## Arithmetic

```bash
X=$((5 + 3))          # 8
Y=$((X * 2))          # 16
Z=$((10 / 3))         # 3 (integer division)

# Operators: + - * / % (parentheses for grouping)
# Comparisons return 1/0: > < >= <= == !=
echo $((5 > 3))       # 1
```

## Functions

```bash
# POSIX style
greet() {
    echo "Hello, $1!"
}

# Bash style
function greet {
    echo "Hello, $1!"
}

greet "Amy"           # → Hello, Amy!
```

## Error Handling

```bash
set -e                # exit on first error

cmd || {
    echo "failed"
    exit 1
}

source utils.kai      # load script (shared scope)
```

## Aliases

```bash
alias ll='ls -la'     # define alias
alias                 # list all aliases
alias ll              # show one alias
unalias ll            # remove alias
```

Aliases expand the first word of a command before parsing. Not recursive.

## Background Jobs

```bash
slow-task &           # run in background
jobs                  # list jobs
wait                  # wait for all
wait %1 %2            # wait for specific
```
