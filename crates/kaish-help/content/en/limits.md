# kaish Known Limitations

## Intentionally Missing

| Feature | Workaround |
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
`&&`/`||`/`!` in one construct. Reach for `test` for muscle memory or where a
plain command is wanted — `test -f x && echo yes`, `if test "$a" = "$b"; then`.

## Lexer/Parser Limitations

| Limitation | Details | Workaround |
|-----------|---------|------------|
| `[[ ]]` parsed as two brackets | Two separate `[` tokens, not a compound keyword | Works for tests; the two-token design deliberately reserves `[ ]` for kaish's native list literals |
| Statement-opening keywords as bare arguments | `echo if` / `echo for` / `echo while` / `echo case` are parse errors (keyword starts a statement). Closers (`done`, `then`, `fi`) are fine. | Quote: `echo "if"` |
| No token-pasting of adjacent unquoted words | `$VAR`/`$(cmd)`/globs are separate words. Unquoted text glued to an expansion (`echo $dir/f`, `echo /tmp/$(id -u).x`, `> $dir/f`) is a **parse error**, not a silent splat. Single-token words (`file.txt`, `v1.2.3`) are fine. | **Quote the whole word**: `"$dir/f"`, `"/tmp/$(id -u).x"`. See `help syntax` → Quoting. |

## Builtin Constraints

| Builtin | Limitation |
|---------|------------|
| `alias` | First word only; not in pipelines or compound commands |
| `set` | `-e`, `-o latch`, `-o trash`, `-o glob`, `-o output-limit[=SIZE]` (no `-u`, `-x`, `pipefail`) |
| `rm` (trash) | Trash failure = error, no fallthrough to permanent delete. Dirs always trash (stat size unreliable). |
| `rm` (latch) | Nonces scoped to (command, paths). Subset confirmation only. 60s TTL. Persist within an embedder session, not across reconnects. |
| `ps` | Linux-only (reads `/proc`) |
| `head`/`tail -c` | Counts bytes (POSIX); can split multi-byte UTF-8 — prefer `-n` for text |
| `**` globs | Slow on deep trees; use specific prefixes |
| `kaish-ignore` | Runtime changes don't persist across sessions; use `~/.kaishrc` or `--init` |
| `kaish-output-limit` | Runtime changes don't persist across sessions; use `~/.kaishrc` or `--init` |

## Execution

- **Pipeline stages run concurrently** with isolated scopes (like bash subshells). Variable assignments in one stage aren't visible in others. Last stage syncs back to parent.
- **Scatter results in completion order**, not input order.
- **Command substitution runs in redirect targets and here-doc bodies** — `cmd > $(gen-path)`, `cat < $(find-cfg)`, and `$(...)` inside a here-doc body all work. The target is a single word, so quote it when it mixes text with an expansion: `> "/tmp/$(id -u).log"`, not `> /tmp/$(id -u).log`.
- **Preprocessor is context-unaware** — `$(( ))` and heredoc markers replaced before parsing.

## External Commands

| Constraint | Workaround |
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
