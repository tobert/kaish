# awk — Field & Record Processing

kaish's `awk` is a focused subset of POSIX awk for line-oriented text:
split each record into fields, match patterns, compute, print. It is a
one-liner tool, not a general-purpose language — see "Not supported" below
and use the suggested kaish alternative instead. The boundary is loud: an
unsupported construct errors with a hint, it never silently does nothing.

## Quick start

```bash
awk '{print $2}' file.txt                 # 2nd whitespace field of each line
awk -F: '{print $1}' /etc/passwd          # field separator ':'  (also -F, -F'\t')
awk 'NR==1 {print}' file.txt              # first line only  (NR = record number)
awk '$3 > 100 {print $1, $3}' data.txt    # filter on a numeric field
awk '/error/ {print}' log.txt             # lines matching a regex
awk '{sum += $3} END {print sum}' d.txt   # accumulate, print at end
awk 'BEGIN {print "head"} {print} END {print "tail"}'
```

A program is `pattern { action }` rules. An omitted pattern runs on every
record; an omitted action defaults to `{ print $0 }`. `BEGIN`/`END` blocks
run before/after all input.

## Fields, records, separators

```bash
$0            # whole record;  $1..$NF  fields;  $(NF-1) computed index
NF NR         # field count / record number     FILENAME  input name
-F:           # set FS on the command line (-F, -F: -F';' -F'\t' all work)
BEGIN{FS=":"; OFS="\t"}     # or set FS/OFS in BEGIN
{$2="X"; print}            # assigning a field rebuilds $0 using OFS
```

Default FS splits on runs of whitespace (leading/trailing trimmed). A
multi-char FS is an ERE regex. Range patterns work: `/start/,/stop/`.

## Strings & substitution

These DO populate / mutate (a common awk pitfall elsewhere — here they work):

```bash
n = split($0, a, ",")      # fills a[1]..a[n], returns n
sub(/foo/, "bar")          # first match in $0; returns 1/0; & = matched text
gsub(/o/, "0")             # every match; returns count; \& for a literal &
gsub(/x/, "y", $3)         # mutate a specific field/var instead of $0
if (match($0, /[0-9]+/))   # sets RSTART (1-based, 0 if none) and RLENGTH (-1 if none)
    print substr($0, RSTART, RLENGTH)
length($1)  length         # string length; bare `length` means length($0)
substr(s,2,3) index(s,t) toupper(s) tolower(s) sprintf("%05.2f", x)
```

Regex is the Rust `regex` crate (ERE-like), not gawk's engine:
**no backreferences** in patterns, and a few escapes differ. Stick to
character classes, `*+?`, `{m,n}`, anchors, and alternation.

## Control flow, arrays, output

```bash
if (c) {...} else {...}     while (c) {...}     for (i=0;i<n;i++) {...}
for (k in arr) {...}        delete arr[k]       "x" in arr
next                        # skip to next record
exit                        # stop now (END still runs)
print a, b                  # OFS between args, ORS at end
printf "%-8s %3d\n", s, n   # C-style format
```

## Not supported (and what to do instead)

Each errors loudly with a hint. Reach for the kaish form on the right.

| awk construct | Use instead |
|---|---|
| `getline` | pipe input in: `cat f \| awk '…'` |
| `print > "file"`, `>>` | pipe kaish's output: `awk '…' \| tee file` |
| `print \| "cmd"`, `"cmd" \| getline` | pipe through kaish: `awk '…' \| cmd` |
| user functions `function f(){…}` | keep it a one-liner, or use a kaish function |
| multi-subscript `a[i,j]` / `SUBSEP` | build a string key: `a[i "," j]` |
| `sin cos atan2 exp log rand srand` | (only `int` and `sqrt` are provided) |
| `ENVIRON` / `ARGV` / `ARGC` | kaish is hermetic; pass values with `-v NAME=VAL` |
| regex backreferences | rewrite without them (regex-crate limitation) |

`-v NAME=value` sets a variable before the program runs. Input is one file
argument or stdin (a pipe). Text only — awk errors on binary input.
