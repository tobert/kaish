# kaish Known Limitations

## Intentionally Missing

| Feature | Use instead |
|---------|------------|
| Brace expansion `{a,b,c}` | List items explicitly |
| Process substitution `<(cmd)` | `cmd > /tmp/t.txt; cmd2 /tmp/t.txt` |
| Backticks `` `cmd` `` | `$(cmd)` |
| `eval` | Write explicit code |
| Implicit word splitting on whitespace | `split "$VAR"` |
| `[ … ]` single-bracket conditional | `test …` or `[[ … ]]` |

- `test` is a VFS-aware builtin that shares semantics with `[[ … ]]`.
- kaish reserves `[` and `]` for `[[ … ]]` conditionals and native list literals like `xs=[a b c]`.
- `[ -f x ]` is a parse error.
- A bare `]` in an argument is also a parse error. Quote or escape it as `ls "clip].mp4"` or `ls clip\].mp4`.
- Prefer `[[ … ]]`. The parser validates its operators, expansions, and compound logic before execution.
- Use `test` when you need a plain builtin command, such as `test -f x && echo yes`.
- See `help syntax` → Test Expressions and `help collections`.

## Lexer/Parser Limitations

| Limitation | Details | Use instead |
|-----------|---------|------------|
| No `until` loop | `until cmd; do …; done` is a parse error. | `while ! cmd; do …; done` |
| Statement-opening keywords as bare arguments | `echo if`, `echo for`, `echo while`, and `echo case` are parse errors because those keywords open statements. Closers like `done`, `then`, and `fi` work unquoted. | Quote the word: `echo "if"` |
| No unquoted token-pasting | `$VAR`, `$(cmd)`, and globs are separate words. Unquoted text adjacent to an expansion such as `echo $dir/f` is a parse error. Single words like `file.txt`, `sed -n 1,3p file`, and `-Wl,-rpath,/x` need no quotes. | Quote the whole word: `"$dir/f"`, `"/tmp/$(id -u).x"`. See `help syntax` → Quoting. |

## Builtin Constraints

| Builtin | Limitation |
|---------|------------|
| `alias` | First word only. Not expanded in pipelines or compound commands. |
| `set` | Supports `-e` and `-o` for `pipefail`, `trash`, `glob`, `crossmounts`, and `output-limit[=SIZE]`. Ignores `-u` and `-x`. An unknown `-o` name exits 2. |
| `rm` with trash | Trash failure returns an error with no permanent delete fallback. Directories always move to Trash. |
| `ls`, `find`, `glob` | A filename containing a newline exits 2 in text output and names the path and `--json`. Pass `--json` to read such names losslessly. |
| `ps` | Reads `/proc` on Linux only. |
| `find` expressions | Allows at most 256 expression nodes and 64 nested groups or negations. Larger expressions exit 2. Split large expressions into separate searches. |
| `cat -A`, `-v`, `-E`, `-T` | Buffers input before marking bytes. `-A` equals `-vET`. Using `-n` with `-E` or `-T` requires UTF-8 text unless `-v` is also set. |
| `edit` | Accepts existing UTF-8 text files only. The kernel intercepts `--json` and `--help` unless you place `--` before the path. |
| `head -c` and `tail -c` | Counts bytes and can split multi-byte UTF-8 characters. Prefer `-n` for text. |
| `**` globs | Slow on deep directory trees. Use specific path prefixes instead. |
| `kaish-ignore` | Runtime changes apply to the current session only. Configure persistent rules in `~/.config/kaish/init.kai`, `~/.kaishrc`, or `$KAISH_INIT`. |
| `kaish-output-limit` | Runtime changes apply to the current session only. Configure persistent limits in `~/.config/kaish/init.kai`, `~/.kaishrc`, or `$KAISH_INIT`. |

## Unsupported Flags

A flag in this table exits 2 with a hint showing the replacement command:

```
cp -p a.txt b.txt
cp: -p is not supported: kaish cannot copy a file's mode, owner, or times; no builtin sets them from another file. Run `cp SRC DST`; the copy gets the current time.
```

Any other unknown flag exits 2 and points to `help NAME` for supported flags.

| Flag | Reason | Use instead |
|------|--------|-------------|
| `cp -p`, `--preserve` | `cp` does not copy mode, owner, or timestamps | `cp SRC DST` |
| `cp -a`, `--archive` | `cp` does not copy mode, owner, or timestamps | `cp -rP SRC DST` |
| `cp -f`, `--force` | `cp` already overwrites existing destinations | `cp SRC DST` |
| `cp -i`, `--interactive` | `cp` never prompts | `cp -n SRC DST` |
| `mv -f`, `--force` | `mv` already overwrites existing destinations | `mv SRC DST` |
| `mv -i`, `--interactive` | `mv` never prompts | `mv -n SRC DST` |
| `mkdir -m`, `--mode` | `mkdir` does not set file modes | `mkdir DIR` |
| `echo -e` | Double-quoted strings already expand `\n` and `\t` | `echo "a\tb"` or `printf 'a\tb\n'` |
| `printf -v` | Assign variables with command substitution | `NAME=$(printf FORMAT ARGS)` |
| `grep -P`, `--perl-regexp` | `grep` reads GNU BRE or ERE without look-around or back-references | `grep -E '[0-9]+' FILE` |
| `find -delete` | `find` only prints matching paths | `for f in $(find . -name '*.tmp'); do rm "$f"; done` |
| `find -exec`, `-execdir`, `-ok`, `-okdir` | `find` only prints matching paths | `for f in $(find . -name '*.txt'); do wc -l "$f"; done` |

## Execution

- **Pipeline stages run concurrently.** Each stage runs in an isolated scope. Variable assignments in earlier stages do not affect later stages or the parent scope. Only the final stage syncs back to the parent.
- **Compound pipeline stages buffer output.** Brace groups and `for`, `while`, `if`, and `case` statements run to completion before passing output to the next stage. Simple command stages still stream.
- **Scatter pipelines forbid compound stages.** Mixing `scatter` or `gather` with a compound stage exits 2. Run the compound separately and pipe its output into `scatter`.
- **Scatter preserves item order.** `gather` outputs rows in input item order rather than completion order.
- **Redirects and here-docs expand `$(…)`.** Both `cmd > $(gen-path)` and `$(…)` inside a here-doc body run command substitution. Quote a redirect target when mixing literal text and expansions, such as `> "/tmp/$(id -u).log"`.
- **Redirects open targets before running the command.** A target failure exits 1 and skips the command or compound body.
  - Redirects never create missing parent directories. Run `mkdir -p` first when the backend allows creating the parent.
  - Paths outside any writable mount report a writable mount path, or say none is available.
  - Read-only mounts and read-only parent directories are refused. See `help vfs`.
- **Compound redirects require a body command and refuse here-docs.** Invalid compound redirects exit 2. Run `: > file` to create an empty file, or pipe a here-doc into the compound. Stdin returns to the session after the body finishes.
- **Merged streams preserve recorded write order.** Redirects like `2>&1`, `1>&2`, and `&>` interleave captured stdout and stderr in write order when spans are available, falling back to stdout then stderr after a spill or overflow. When stderr already wrote to a background job stream, `1>&2` appends stdout after published stderr. See `docs/LANGUAGE.md`, "Pipes & Redirects".
- **Reading and writing the same file is refused.** Commands like `sort < f > f` and `cat < f >> f` exit 1. Write to a temporary file first, then `mv` or append it.
- **Recursion depth is capped at 48.** Nested `$(…)`, recursive functions, and sourced `.kai` scripts stop with `maximum recursion depth exceeded` at depth 48.

## External Commands

| Constraint | Use instead |
|-----------|------------|
| No PTY allocated | Run interactive programs from a real terminal. Inherited TTYs work. |
| Non-pipeline output buffered | Redirect to a file or run in a pipeline |
| Virtual working directory fails | Run `cd` to a real directory first |
| External commands bypass VFS sandbox | Set `allow_unwrapped_commands=false` to block `PATH` lookup, `exec`, `spawn`, and `env CMD` |
