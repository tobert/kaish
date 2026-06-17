# kaish (会sh)

<p align="center">
  <img src="docs/banner.svg" alt="Kai the hermit crab — kaish mascot — looking at kaish code" width="720">
</p>

Kaish is a predictable Bourne-like shell for AI agents — embeddable in Rust and available as an
MCP server. It ships many in-process builtins and a virtual filesystem that can be bridged to an
underlying system or completely sandboxed.

## Install

```bash
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

```bash
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
| **Builtins** | grep, jq, git, find, sed, awk, diff, patch, and more — all in-process |
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
| **Files** | basename, cat, cd, checksum, cmp, cp, dd, dirname, find, glob, ln, ls, mkdir, mktemp, mv, patch, pwd, readlink, realpath, rm, stat, tee, touch, tree, write |
| **JSON** | jq |
| **Git** | git (init, clone, status, add, commit, log, diff, branch, checkout, worktree) |
| **System** | alias, bg, date, echo, env, exec, export, fg, help, hostname, jobs, kill, printf, ps, read, seq, set, sleep, spawn, test/\[\[, timeout, tokens, uname, unalias, unset, wait, which |
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

```bash
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
Nonces persist within a session — in the REPL across commands, in MCP across
`execute()` calls. Each new connection starts a fresh nonce store.
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

```bash
$ kaish
kaish> for f in *.rs; do wc -l "$f"; done
  142 main.rs
   87 lib.rs
kaish>
```

### kaish-mcp

MCP server exposing kaish to AI agents as a single tool, `execute`. Help
content ships as MCP prompts (`kaish-overview`, `kaish-syntax`, …), and VFS
files are addressable as MCP resources (`kaish://vfs/{path}`).

#### Installation

Add to your MCP client configuration:

```json
{
  "mcpServers": {
    "kaish": {
      "command": "kaish-mcp",
      "args": ["--init", "/home/you/.config/kaish/agent.kai"]
    }
  }
}
```

The `--init <path>` flag loads a `.kai` script before every `execute` call —
aliases, safety options, environment setup. Repeatable (multiple `--init` flags
load in order). Hot-reloaded: edit the file, next call picks up changes without
restarting. Omit `args` entirely if no init scripts are needed.

#### The `execute` tool

Runs a kaish script: pipes, redirects, here-docs, if/for/while, functions,
`${VAR:-default}`, `$((arithmetic))`, scatter/gather parallelism. Not
supported: process substitution `<()`, backticks, `eval`. Native paths work
within `$HOME`; `/tmp` for interop; `/v/` is in-memory scratch.

Every call returns human-readable text content plus a machine-readable
`structured_content` envelope:

```json
{
  "code": 0,
  "ok": true,
  "stdout": "...",
  "stderr": "...",
  "data": null,
  "output": null,
  "content_type": null,
  "baggage": {}
}
```

Output is clean text by default — simple commands return plain text,
structured builtins (`ls`, `kaish-mounts`, `kaish-vars`) render readable
tab-separated values. Add `--json` to any command to get JSON on stdout and
the parsed value in `data`; `output` carries the renderable structured form
(tables and trees) from structured builtins. `data` is only ever set
explicitly by builtins — kaish never infers it by sniffing stdout. When `data` or `output` already
carry the same information, `stdout` is omitted from the envelope; the text
content blocks always have the rendered form.

Exit codes agents can branch on:

| `code` | Meaning | Recovery |
|--------|---------|----------|
| 0 | Success | — |
| 1 | Failure | Read `stderr` |
| 2 | Confirmation required (`set -o latch`) | Re-run with `--confirm="<nonce>"` — the nonce is in the `To confirm, run:` line and in `data` |
| 3 | Output truncated by the output limit | `original_code` holds the real exit code; the message names the spill file — `cat` it, or narrow the query |
| 124 | Timeout (`timeout_ms`, default 30 s) | — |
| 130 | Cancelled | — |

Per-call lifecycle — each `execute` call gets a fresh kernel:

| Resets every call | Persists across calls |
|-------------------|-----------------------|
| Variables, functions, aliases | Confirmation nonces (60 s TTL) |
| Working directory (`cwd` param, default `$HOME`) | Trash contents |
| `set -o` options | Init scripts (re-read each call — edits hot-reload) |
| `--overlay` writes (commit in the same call) | |

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
- Help is in-band: `help builtins`, `help syntax`, `help <tool>` — or use
  the MCP prompts.

#### Why an MCP shell?

AI agents need to compose operations — filter outputs, transform data, iterate over results.
Individual MCP tool calls are atomic operations; kaish lets agents combine them:

```bash
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

```bash
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
