# kaish (会sh)

`kaish` fuses Bourne shell syntax with typed JSON values and structured output.
The kernel validates every script before execution.
Builtins run inside the kernel process.
External commands run through `PATH`.
An embedder can also register a wrapped command.
A wrapped command pairs an external program with a declared grammar and runs even when `PATH` lookup is off.
It appears in `help builtins`, and `help <name>` lists its allowed verbs and flags.
Any other verb or flag fails before spawning and names the allowed set.

## Topics

```
help syntax        Variables, quoting, pipes, control flow
help collections   Lists & records: literals, access, iteration, lvalues
help builtins      List of available builtins
help vfs           Virtual filesystem mounts and paths
help overlay       Copy-on-write overlay mode and kaish-vfs
help scatter       Parallel processing (散/集)
help ignore        Ignore file configuration
help output-limit  Output size limit configuration
help limits        Known limitations
help <tool>        Help for one tool, such as help grep
```

## Output Formats

Builtins return structured data.
Pass `--json` for machine-readable output:

```sh
ls --json                # JSON array of file nodes
kaish-vars --json        # JSON array of {NAME, VALUE} objects
ps --json                # JSON array of process info
ls --json=$WANT_JSON     # toggle JSON output from a variable
```

`--json=VALUE` is off for `0`, `false`, and an empty value.
Any other value turns `--json` on.
Every builtin reads this flag the same way.

```sh
echo hi --json                 # "hi\n"
grep --json nomatch file       # {"code":1,"error":""}
```

A zero exit code prints the JSON value directly, or prints nothing when empty.
A nonzero exit code prints `{"code":N,"error":"…"}`, even when the command wrote no error.
Partial structured results appear under `data`, and plain text appears under `output`.
Check the exit code first.
For example, `diff --json` on differing files exits 1 and places its diff under `data`.

Only the final builtin stage formats output as JSON.
Earlier pipeline stages pass streams to the next stage.
External commands receive `--json` as a literal argument.
Refusals before builtin dispatch bypass this formatter.
Oversize output spills to a file and exits 3 with a truncated preview and spill metadata.
That preview may contain incomplete JSON.
See `help limits`.

A row read from a file line carries a 1-based integer `line` field:

```sh
grep -n fn main.rs --json | jq '.[].line'
head -n 3 main.rs --json                  # [{"TEXT":"…","line":1,"hash":"d246"}, …]
cat -n main.rs --json | jq 'map(select(.line > 40))'
```

`grep` always sets `line`, even without `-n`.
The `-n` flag changes text formatting, not the JSON anchor.
`tail` reports each line's 1-based position in the file, not its row index in the output.
Rows from non-line data, such as file listings or processes, omit `line` completely.
Test `has("line")` in `jq` to distinguish line rows from other rows.

Pass `--hashline` to print rows as `LINE:HASH:TEXT`.
`edit` accepts `LINE:HASH` as a line anchor:

```sh
cat --hashline main.rs                    # 1:d246:use std::io;
grep --hashline TODO src/a.rs src/b.rs    # src/b.rs:12:5b0b:    // TODO: retry
```

Anchors exist only for lines read from a named file.
`--hashline` exits 2 on stdin, on multiple files passed to `cat`, `head`, or `tail`, and on unanchored output like `ls --hashline`.
With `--json`, each anchored row also carries `hash`.
See `docs/LANGUAGE.md`, "Line anchors (`--hashline`)".

## Quick Examples

```sh
# Familiar syntax: globs, pipes, and control flow
ls *.txt | wc -l
for f in src/*.rs; do grep "TODO" "$f"; done

# Variables and conditionals
NAME="world"
if [[ -n $NAME ]]; then echo "Hello, ${NAME}!"; fi

# Parallel execution with scatter and gather
seq 1 100 | scatter --as N --limit 4 | process $N | gather
```
