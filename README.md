# kaish (会sh)

<p align="center">
  <img src="docs/banner.svg" alt="Kai the hermit crab — kaish mascot — looking at kaish code" width="720">
</p>

Kaish is a predictable shell as a rust library. It includes builtin commands so it does
not need `fork()` or `exec()` to perform most text processing tasks. Kaish has its
own virtual filesystem layer that can be passed through to a regular filesystem, or
work completely in-memory. This enables kaish to be sandboxed from its host while
providing a complete shell scripting environment for agents without backflips to contain
and parse the output of a traditional shell.

## Install

```sh
cargo install kaish
```

This is preferred for now while kaish is still experimental. Containers and binaries
are in future plans when things stabilize a bit more and I have time (or PRs!).

## Why kaish?

Kaish is sh-like but not a full Bourne shell or bash. The idea is to preserve the language
that's in our muscle/model memory, while providing better pre-execution syntax checking,
easy embedding, and a VFS abstraction to help with sandboxing.

- **No implicit word splitting** — `$VAR` is always one value, never split on spaces
- **Quote to join** — kaish doesn't paste adjacent unquoted tokens; quote interpolated words: `"$dir/file.txt"`, `"out-$(date +%s).log"`
- **Bare glob expansion** — `ls *.txt` works; opt out with `set +o glob`
- **Structured iteration** — `for i in $(seq 1 5)` works via structured data, not word splitting
- **Line iteration in for-loops** — `for line in $(cat file)` splits on `\n` only; whitespace within a line is never split
- **Explicit splitting** — use `split "$VAR"` for whitespace/delimiter/regex splitting
- **No backticks** — only `$(cmd)` substitution
- **Strict booleans** — `TRUE` and `yes` are errors, not truthy
- **Pre-validation** — catch errors before execution, not at runtime

Skills transfer from bash. Footguns (hopefully) don't.

## Quick Tour

```sh
#!/usr/bin/env kaish

# Familiar bash-style syntax
GREETING="Hello"
echo "$GREETING, world!"

# Control flow
if [[ -f config.json ]]; then
    echo "Config found"
fi

# For loops - no implicit *word* splitting
for item in one two three; do      # literal items
    echo "Processing: $item"
done

for i in $(seq 1 3); do            # structured data iteration
    echo "Count: $i"
done

for line in $(cat hosts.txt); do   # multi-line stdout splits on \n
    echo "host: $line"
done

for file in *.txt; do              # bare glob expansion
    echo "Found: $file"
done

# Quote to join: adjacent unquoted tokens never paste together
echo "$GREETING/world.txt"          # ✅  quote the whole word
# echo $GREETING/world.txt          # ❌  parse error — kaish won't paste $GREETING and /world.txt

# Pipes and redirects
cat urls.txt | grep "https" | head -n 10 > filtered.txt

# Here-strings: feed a variable straight into stdin — jq is built-in, so
# this replaces bash's `echo "$R" | jq` without spawning a subprocess
RESULT='{"name":"amy"}'
jq -r '.name' <<< "$RESULT"

# jq also speaks --arg / --argjson / -n (matches real jq's CLI):
jq -n --argjson r "$RESULT" -r '$r.name'

# Glob patterns expand inline, or use the glob builtin for options
glob "**/*.rs" --exclude="*_test.rs"

# Parallel execution with scatter/gather — --as N binds $N in each worker; --limit caps concurrency
seq 1 10 | scatter --as N --limit 4 | echo "processing $N" | gather
```

## Language Features

| Feature | Description |
|---------|-------------|
| **Familiar syntax** | Variables, pipes, control flow, functions — Bourne-inspired, modern semantics |
| **Builtins** | grep, jq, find, sed, awk, diff, patch, and more — all in-process |
| **Structured data** | Commands return typed arrays — `for i in $(seq 1 5)` iterates 5 values, not word-split text |
| **Strict validation** | Errors caught before execution with clear messages |
| **Virtual filesystem** | Unified access: native `$HOME` paths (sandboxed), `/tmp`, and the in-memory `/v/` namespace (`/v/jobs` observability, scratch storage) |
| **Scatter/gather** | Built-in parallelism with 散/集 |

See [Language Reference](docs/LANGUAGE.md) for complete syntax. Use `help builtins` or `help <tool>` for per-tool docs.

---

## Builtins

kaish builtins run in-process — no subprocesses, no PATH lookups, no platform
variance. They exist because agents need tools they can verify: a `grep` that behaves identically
everywhere, a `jq` that always uses the same filter syntax, an `awk` that never surprises.

**Design principles:**

- **Verifiable** — each builtin has a schema (params, types, examples) exposed via `help <tool>`.
  Agents can introspect before calling.
- **Convention-following** — flags and behavior match the patterns deeply embedded in training data
  and decades of existing scripts. `grep -rn`, `sed 's/old/new/g'`, `awk '{print $1}'` all work
  as expected.
- **80/20** — implement the features used 80% of the time, deliberately omit the 20% that add
  complexity without proportional value. Missing features compose via pipes.
- **ERE everywhere** — all regex uses Extended Regular Expressions. No BRE/ERE confusion.

| Category | Tools |
|----------|-------|
| **Text** | awk, base64, cut, diff, grep, head, sed, sort, split, tac, tail, tr, uniq, wc, xxd |
| **Files** | basename, cat, cd, checksum, cmp, cp, dd, dirname, file, find, glob, ln, ls, mkdir, mktemp, mv, patch, pwd, readlink, realpath, rm, stat, tee, touch, tree, write |
| **JSON** | fromjson, jq, keys, tojson, values |
| **System** | alias, bg, date, echo, env, exec, export, fg, help, hostname, jobs, kill, printf, ps, read, seq, set, sleep, spawn, timeout, tokens, uname, unalias, unset, wait, which |
| **Parallel** | scatter, gather |
| **Meta** | assert, false, true |
| **kaish-*** | kaish-ast, kaish-clear, kaish-ignore, kaish-last, kaish-mounts, kaish-output-limit, kaish-status, kaish-tools, kaish-trash, kaish-validate, kaish-vars, kaish-version, kaish-vfs |

---

## Configuration

The REPL loads an init file on startup (first match wins):

1. `$KAISH_INIT` (env var)
2. `~/.config/kaish/init.kai`
3. `~/.kaishrc`

Use it for aliases, exports, and a custom prompt:

```sh
# ~/.config/kaish/init.kai
alias ll='ls -la'
alias gs='git status'
export EDITOR=vim

kaish_prompt() {
    echo "$(pwd)> "
}
```

The `kaish_prompt` function is called before each input line. If not defined,
the default `会sh> ` prompt is used.

### Environment Variables

| Variable | Effect |
|----------|--------|
| `KAISH_INIT` | Path to init script (overrides default locations) |
| `KAISH_LATCH=1` | Enable confirmation latch — `rm` requires nonce confirmation |
| `KAISH_TRASH=1` | Enable trash-on-delete — `rm` moves files to freedesktop.org Trash |

Latch and trash can also be toggled at runtime with `set -o latch` / `set -o trash`.

**Latch details:** Nonces are scoped to command + path — a nonce issued for `rm fileA`
cannot confirm `rm fileB`. Confirmed paths must be a subset of authorized paths.
Nonces persist within a session — in the REPL across commands, and across an
embedder's `execute()` calls. Each new connection starts a fresh nonce store.
Embedders control this via `KernelConfig::with_nonce_store()`.

**Trash details:** Files under 10MB and all directories go to trash (configurable via
`kaish-trash config max-size`). Excluded paths (`/tmp`, `/v/*`) bypass trash. If
`trash::delete` fails, `rm` returns an error — it never silently falls through to
permanent delete.

---

## Components

kaish is built as a set of crates that can be used independently:

### kaish-kernel

The core execution engine. Lexer, parser, interpreter, builtins, VFS.

```rust
use kaish_kernel::{Kernel, KernelConfig};

let kernel = Kernel::new(KernelConfig::default())?;
let result = kernel.execute("echo hello | tr a-z A-Z").await?;
println!("{}", result.text_out());  // "HELLO"
```

The kernel is embeddable — no external dependencies, no subprocess spawning for builtins.

### kaish-repl

Interactive shell with readline support, history, and tab completion.

```sh
$ kaish
kaish> for f in *.rs; do wc -l "$f"; done
  142 main.rs
   87 lib.rs
kaish>
```

### Embedding

kaish is built to be embedded: construct a `Kernel`, give it a backend, and call
`execute()`. The kernel is hermetic (it never reads ambient OS env — the frontend
supplies vars), the VFS can be bridged to the host or fully sandboxed, and the
OS-touching capability features (`subprocess`, `host`, `os-integration`, `tokens`)
are opt-in so the dangerous surface is named, not inherited. The default build is
real-file I/O plus an in-memory overlay — nothing that spawns a process or reads the
host. See [docs/EMBEDDING.md](docs/EMBEDDING.md) for the full guide.

**Using kaish as an MCP server?** kaish core doesn't ship one — the showcase is
[**kaibo**](https://github.com/tobert/kaibo) (解剖), a read-only codebase-analysis
MCP that drives kaish to read and reason about a project and answers with cited
`file:line` spans. [**kaijutsu**](https://github.com/tobert/kaijutsu) embeds kaish
behind its own MCP interface too. Both show the pattern: embed the kernel, then
expose it however your agent needs.

Every `execute()` returns an `ExecResult`. Output is clean text by default — simple
commands return plain text, structured builtins (`ls`, `kaish-mounts`, `kaish-vars`)
render readable tab-separated values, and `--json` on any command emits JSON plus a
parsed value (`data`) that builtins set explicitly — kaish never infers it by
sniffing stdout. The exit code is something agents can branch on:

| `code` | Meaning | Recovery |
|--------|---------|----------|
| 0 | Success | — |
| 1 | Failure | Read `stderr` |
| 2 | Confirmation required (`set -o latch`) | Re-run with `--confirm="<nonce>"` — the nonce is in the `To confirm, run:` line and in `data` |
| 3 | Output truncated by the output limit | `original_code` holds the real exit code; the message names the spill file — `cat` it, or narrow the query |
| 124 | Timeout (`timeout_ms`, default 30 s) | — |
| 130 | Cancelled | — |

Embedders typically run a fresh kernel per request (variables, functions, aliases,
`set -o` options, and `cwd` reset each time) while trash and confirmation nonces
(60 s TTL) persist — see [docs/EMBEDDING.md](docs/EMBEDDING.md) for the lifecycle and
`ExecuteOptions`.

#### Sandbox & safety

- Builtins go through the VFS, sandboxed to `$HOME` + `/tmp`; `/v/` is
  in-memory scratch with a 64 MiB per-call budget.
- **External commands resolved via `PATH` run against the real filesystem**
  — the VFS sandbox does not apply to them. Block them entirely with
  `allow_external_commands=false`, or build without the `subprocess`
  capability feature.
- `--overlay` makes the sandbox copy-on-write for the call: writes stay in
  memory unless the script runs `kaish-vfs commit` (same call — overlays
  don't persist across calls).
- `set -o latch` gates destructive commands behind exit-2 nonce
  confirmation; `set -o trash` diverts deletes to the trash instead.

#### Agent gotchas

- **Quote to join.** Unlike bash, kaish never concatenates adjacent unquoted
  tokens. `echo $dir/out.txt` is a **parse error** (the words would otherwise
  splat into two arguments) — quote the whole word: `echo "$dir/out.txt"`.
  Single-token words like `file.txt` or `v1.2.3` are fine unquoted.
- `FOO=bar cmd` scopes `FOO` to that one command (its environment and arguments),
  like bash — it does **not** persist afterward. A plain `FOO=bar` with no
  command following still sets `FOO` persistently.
- Help is in-band: `help builtins`, `help syntax`, `help <tool>`.

#### Why a shell for agents?

AI agents need to compose operations — filter outputs, transform data, iterate over results.
A single tool call per operation is atomic and chatty; kaish lets agents combine them in one script:

```sh
# Filter and transform in one script
ls src/ | grep "\.rs$" | head -n 5

# Iterate over results
for f in *.json; do
    jq ".name" "$f"
done

# Parallel processing
seq 1 10 | scatter --as N --limit 4 | echo "processing $N" | gather
```

## Why 会sh (kaish)?

会sh was originally prototyped as part of 会術 Kaijutsu and was separate enough
it made sense to split it out. Amy was also a fan of ksh and pdksh back in the 00s
so k-ai-sh seems fun.

---

## Building from Source

```sh
git clone https://github.com/tobert/kaish
cd kaish
cargo build --release
```

## Contributing

Agent-generated PRs are welcome! 🤖 This project is built with AI agents and we
love seeing what other agents come up with. **All changes go through a PR** —
branch, push, and open a PR rather than committing to `main` (releases are the
exception). That said, please have your agent (or another model) review the PR
before submitting — a few tokens on review goes a long way. Same goes for issues:
agent-filed is fine, just make sure it makes sense.

If you're working with AI coding agents, you might also be interested in:

- [**gpal**](https://github.com/tobert/gpal) — Gemini as an MCP server (pairs well with Claude Code)
- [**cpal**](https://github.com/tobert/cpal) — Claude as an MCP server (pairs well with Gemini CLI)

## License

MIT
