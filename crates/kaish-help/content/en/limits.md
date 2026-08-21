# kaish Known Limitations

## Intentionally Missing

| Feature | Use instead |
|---------|------------|
| Shell brace expansion `{a,b,c}` | List items explicitly |
| Process substitution `<(cmd)` | `cmd > /tmp/t.txt; cmd2 /tmp/t.txt` |
| Backticks `` `cmd` `` (lexer error, not silently tolerated) | `$(cmd)` |
| `eval` | Write explicit code |
| Implicit word splitting on whitespace | `split "$VAR"` (for-loop `$(cmd)` does split on newlines — see Bash vs kaish below) |
| `[ … ]` single-bracket conditional | `test …` or `[[ … ]]` |

`test` is a real builtin (VFS-aware, following `[[`'s semantics), so both
`test -f x` and `[[ -f x ]]` work. There is no `[` command, though: the bracket
belongs to kaish — `[[ … ]]` and native list literals (`xs=[a b c]`, see
`help syntax` → Collections) — so `[ -f x ]` is a parse error. Prefer `[[ … ]]`:
it is real syntax the parser understands, so kaish can **validate it before
running** (catch a malformed test, an unknown operator, an unquoted expansion),
and it carries the richer tests (membership, regex, shape guards) plus compound
`&&`/`||`/`!` in one construct. Use `test` when you want a plain command, or
when the `sh` habit is faster to type — `test -f x && echo yes`,
`if test "$a" = "$b"; then`.

## Lexer/Parser Limitations

| Limitation | Details | Use instead |
|-----------|---------|------------|
| No `until` loop | `until cmd; do …; done` is a parse error. Deliberate — `while !` says the same thing and kaish keeps one spelling per idea. Reconsidered if a use case turns up that `while !` reads badly for. | `while ! cmd; do …; done` |
| `[[ ]]` parsed as two brackets | Two separate `[` tokens, not a compound keyword | Works for tests; the two-token design deliberately reserves `[ ]` for kaish's native list literals |
| Statement-opening keywords as bare arguments | `echo if` / `echo for` / `echo while` / `echo case` are parse errors (keyword starts a statement). Closers (`done`, `then`, `fi`) are fine. | Quote: `echo "if"` |
| No token-pasting of adjacent unquoted words | `$VAR`/`$(cmd)`/globs are separate words. Unquoted text glued to an expansion (`echo $dir/f`, `echo /tmp/$(id -u).x`, `> $dir/f`) is a **parse error**, not a silent splat. Single-token words (`file.txt`, `v1.2.3`) are fine. A bare `,` is NOT one of these — it's significant only inside a `[...]`/`{...}` literal or pattern, so `sed -n 1,3p file` and `sort -k 2,2n` need no quoting. | **Quote the whole word**: `"$dir/f"`, `"/tmp/$(id -u).x"`. See `help syntax` → Quoting. |

## Builtin Constraints

| Builtin | Limitation |
|---------|------------|
| `alias` | First word only; not in pipelines or compound commands |
| `set` | `-e`, `-o pipefail`, `-o trash`, `-o glob`, `-o output-limit[=SIZE]` (`-u`, `-x` ignored; an unknown `-o` name exits 1) |
| `rm` (trash) | Trash failure = error, no fallthrough to permanent delete. Dirs always trash (stat size unreliable). |
| `ps` | Linux-only (reads `/proc`) |
| `head`/`tail -c` | Counts bytes (POSIX); can split multi-byte UTF-8 — prefer `-n` for text |
| `**` globs | Slow on deep trees; use specific prefixes |
| `kaish-ignore` | Runtime changes don't persist across sessions; use `~/.kaishrc` or `--init` |
| `kaish-output-limit` | Runtime changes don't persist across sessions; use `~/.kaishrc` or `--init` |

## Execution

- **Pipeline stages run concurrently** with isolated scopes (like bash subshells). Variable assignments in one stage aren't visible in others. Last stage syncs back to parent.
- **A compound pipeline stage buffers.** `for`/`while`/`if`/`case` may sit in any pipeline position, but such a stage runs to completion before the next stage sees a byte — `for f in $(seq 1 100000); do echo $f; done | head -n 1` runs every iteration where `sh` stops at the first line. Same answer, more work. Command stages still stream.
- **`scatter`/`gather` cannot share a pipeline with a compound stage** — exits 2. Run the compound on its own and pipe its output in.
- **Scatter results are in item order**, never completion order — a row's position identifies its item.
- **Command substitution runs in redirect targets and here-doc bodies** — `cmd > $(gen-path)`, `cat < $(find-cfg)`, and `$(...)` inside a here-doc body all work. The target is a single word, so quote it when it mixes text with an expansion: `> "/tmp/$(id -u).log"`, not `> /tmp/$(id -u).log`.
- **Recursion is depth-capped at 48** — nested `$(...)`, recursive shell functions, and `.kai` scripts sourcing each other are bounded so a runaway (or a missing base case) returns a loud `maximum recursion depth exceeded` error instead of overflowing the stack. Real recursion nests far shallower; this only stops runaways.
- **Preprocessor is context-unaware** — `$(( ))` and heredoc markers replaced before parsing.

## External Commands

| Constraint | Use instead |
|-----------|------------|
| No PTY assumed | TTY works if present, but kaish doesn't allocate one |
| Output buffered (non-pipeline) | Redirect to file or use in pipeline |
| Virtual cwd fails | `cd` to real directory before running |
| Bypass VFS sandbox | Set `allow_external_commands=false` to block; `exec`/`spawn` also gated |

## Bash vs kaish

| Bash | kaish |
|------|-------|
| `for i in $VAR` splits on IFS | E012 validator error; use `$(split "$VAR")` |
| `for i in $(cmd)` splits on IFS (default: any whitespace) | Splits on `\n` only — `for line in $(cat file)` iterates per line; `for x in $(echo "a b c")` iterates once |
| `for i in "$(cmd)"` iterates once | Same — quoted substitution suppresses the per-line split |
| `*.txt` expands at shell | Bare globs expand (disable with `set +o glob`) |
| Regex in `=~` is unquoted | Quotes allowed: `=~ "\.rs$"` |
| `printf "a"; printf "b"` → `ab` | Same — `ab` (no separator inserted; `&&` chains match too) |
