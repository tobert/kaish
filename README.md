# kaish (会sh)

<p align="center">
  <img src="docs/banner.svg" alt="Kai the hermit crab — kaish mascot — looking at kaish code" width="720">
</p>

**kaish** is a predictable shell for AI agents: an embeddable Rust library with a
reference REPL. The language is a strict subset of the `sh` that's already in your
muscle memory — and the models' — so skills transfer from bash. Footguns
(hopefully) don't.

The builtins — grep, sed, awk, find, and ninety-odd more — run in-process, so most
text processing never needs `fork()` or `exec()`. All file I/O goes through a
virtual filesystem that can pass through to the real one, stay entirely in memory,
or overlay copy-on-write between the two. That's the sandbox story: an embedded
kaish gives an agent a complete scripting environment without the backflips it
takes to contain a traditional shell and parse its output.

**Status:** 0.11, pre-1.0. The language has settled; what remains before 1.0 is
ergonomics and correctness polish. Everything ships through
[CHANGELOG.md](CHANGELOG.md).

## Why a shell for agents?

Agents need to compose operations — filter output, transform data, iterate over
results. One tool call per operation is atomic and chatty; a shell does it in one
round trip:

```sh
# Filter and transform in one script
ls src/ | grep "\.rs$" | head -n 5

# Iterate over results
for f in *.log; do
    wc -l "$f"
done

# Parallel processing with bounded concurrency
seq 1 10 | scatter --as N --limit 4 | echo "processing $N" | gather
```

But handing an agent `bash -c` is a rough deal for everyone involved:
word-splitting surprises, tools that vary by platform and version, and full host
access by default. kaish keeps the language the models already know and swaps out
the implementation: strict parsing with pre-execution validation, builtins that
behave identically everywhere, and a filesystem boundary the embedder controls.

Underneath, kaish's data model is JSON. A variable holds an array or a record as
naturally as a string, `$(cmd)` substitution carries structured values — not
text to re-split — and `--json` on any command emits the same typed data the
language works with internally. Structured results flow through pipes,
subscripts, and iteration without a serialization step at every seam.

## Why kaish?

Kaish is sh-like but not a full Bourne shell or bash. The idea is to preserve the
language that's in our muscle/model memory, while providing better pre-execution
syntax checking, easy embedding, and a VFS abstraction to help with sandboxing.

- **No implicit word splitting** — `$VAR` is always one value, never split on spaces
- **Quote to join** — kaish doesn't paste adjacent unquoted tokens; quote interpolated words: `"$dir/file.txt"`, `"out-$(date +%s).log"`
- **Bare glob expansion** — `ls *.txt` works; opt out with `set +o glob`
- **Structured iteration** — `for i in $(seq 1 5)` works via structured data, not word splitting
- **Line iteration in for-loops** — `for line in $(cat file)` splits on `\n` only; whitespace within a line is never split
- **JSON data model** — kaish values *are* JSON values: strings, numbers, booleans, arrays, records. `fromjson`/`tojson` bridge text; `${xs[0]}`, `${r[key]}`, `keys`, `values` index natively
- **Explicit splitting** — use `split "$VAR"` for whitespace/delimiter/regex splitting
- **No backticks** — only `$(cmd)` substitution
- **Strict booleans** — `TRUE` and `yes` are errors, not truthy
- **Pre-validation** — catch errors before execution, not at runtime

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

# The data model is JSON: parse text into typed collections, index directly
CONFIG='{"name":"amy","langs":["rust","kaish"]}'
C=$(fromjson <<< "$CONFIG")
echo "${C[name]} writes ${C[langs][0]}"      # amy writes rust

SERVERS=$(fromjson <<< '{"web1":"10.0.0.1","web2":"10.0.0.2"}')
for host in $(keys $SERVERS); do
    echo "$host -> ${SERVERS[$host]}"
done

# Glob patterns expand inline, or use the glob builtin for options
glob "**/*.rs" --exclude="*_test.rs"

# Parallel execution with scatter/gather — --as N binds $N in each worker;
# --limit caps concurrency; gather emits one JSONL record per worker
seq 1 10 | scatter --as N --limit 4 | echo "processing $N" | gather
```

See [docs/LANGUAGE.md](docs/LANGUAGE.md) for the complete language reference, or
ask kaish itself — help is in-band: `help builtins`, `help syntax`, `help <tool>`.

## Getting Started

You'll need a Rust toolchain ([rustup](https://rustup.rs)). Containers and
prebuilt binaries are in future plans when things stabilize a bit more and I have
time (or PRs!).

### The REPL

```sh
cargo install kaish-repl    # installs a binary named `kaish`
```

```
$ kaish
会sh> for f in *.rs; do wc -l "$f"; done
  142 main.rs
   87 lib.rs
会sh>
```

The REPL loads an init file on startup — the first match of `$KAISH_INIT`,
`~/.config/kaish/init.kai`, `~/.kaishrc` — for aliases, exports, and a custom
prompt. Define `kaish_prompt` and it's called before each input line:

```sh
# ~/.config/kaish/init.kai
alias ll='ls -la'
alias gs='git status'
export EDITOR=vim

kaish_prompt() {
    echo "$(pwd)> "
}
```

### Embedding the kernel

Construct a `Kernel`, point it at a sandbox root, call `execute()`:

```toml
[dependencies]
kaish-kernel = "0.11"
tokio = { version = "1", features = ["full"] }
```

```rust
use kaish_kernel::{Kernel, KernelConfig, VfsMountMode};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Sandboxed to one directory. The default build can't spawn processes
    // at all — external commands are an opt-in cargo feature (`subprocess`).
    let config = KernelConfig::named("my-agent")
        .with_vfs_mode(VfsMountMode::Sandboxed {
            root: Some(PathBuf::from("/path/to/workspace")),
        })
        .with_cwd(PathBuf::from("/path/to/workspace"));
    let kernel = Kernel::new(config)?;

    let result = kernel.execute(r#"ls | grep '\.rs$' | head -n 3"#).await?;
    if result.code != 0 {
        eprintln!("script failed: {}", result.err);
    }
    println!("{}", result.text_out());
    Ok(())
}
```

The kernel is hermetic by default — it never reads the OS environment (the
frontend supplies vars), and the OS-touching capability features (`subprocess`,
`host`, `os-integration`, `tokens`) are opt-in cargo features, so the dangerous
surface is named, not inherited. Every `execute()` returns an `ExecResult` with
clean text output, an optional typed `data` payload (`--json` on any command),
and an exit code agents can branch on: `2` means a destructive op wants
confirmation, `3` means output was truncated, `124` is a timeout.

[docs/EMBEDDING.md](docs/EMBEDDING.md) is the full guide: kernel construction,
capability features, `ExecuteOptions`, custom tools, the exit-code contract, and
thread stack sizing.

**Using kaish over MCP?** kaish core doesn't ship an MCP server — that surface
lives in the embedders. [**kaibo**](https://github.com/tobert/kaibo) (解剖) is the
showcase: a read-only codebase-analysis MCP that drives kaish to read and reason
about a project and answers with cited `file:line` spans.
[**kaijutsu**](https://github.com/tobert/kaijutsu) embeds kaish behind its own MCP
interface too. Both show the pattern: embed the kernel, then expose it however
your agent needs.

## Builtins

kaish builtins run in-process — no subprocesses, no PATH lookups, no platform
variance. They exist because agents need tools they can verify: a `grep` that
behaves identically everywhere, a `sed` whose dialect doesn't depend on the host,
an `awk` that never surprises.

**Design principles:**

- **Verifiable** — each builtin has a schema (params, types, examples) exposed via `help <tool>`.
  Agents can introspect before calling.
- **Convention-following** — flags and behavior match the patterns deeply embedded in training data
  and decades of existing scripts. `grep -rn`, `sed 's/old/new/g'`, `awk '{print $1}'` all work
  as expected.
- **80/20** — implement the features used 80% of the time, deliberately omit the 20% that add
  complexity without proportional value. Missing features compose via pipes.
- **ERE-first regex** — Extended Regular Expressions are the dialect everywhere; common GNU BRE
  spellings (`\|`, `\(…\)`, `\{n,m\}`, `\+`, `\?`) are accepted too and rewrite to ERE, so existing
  scripts keep working. `-E`/`-r` opts into strict ERE.

| Category | Tools |
|----------|-------|
| **Text** | awk, base64, cut, diff, grep, head, sed, sort, split, tac, tail, tr, uniq, wc, xxd |
| **Files** | basename, cat, cd, checksum, cmp, cp, dd, dirname, file, find, glob, ln, ls, mkdir, mktemp, mv, patch, pwd, readlink, realpath, rm, stat, tee, touch, tree, write |
| **JSON** | fromjson, fromjsonl, jq, keys, tojson, tojsonl, typeof, values |
| **System** | alias, bg, date, echo, env, exec, export, fg, help, hostname, jobs, kill, printf, ps, push, read, seq, set, sleep, spawn, timeout, tokens, uname, unalias, unset, wait, which |
| **Parallel** | scatter, gather |
| **Meta** | assert, false, test, true |
| **kaish-*** | kaish-ast, kaish-clear, kaish-ignore, kaish-last, kaish-mounts, kaish-output-limit, kaish-status, kaish-tools, kaish-trash, kaish-validate, kaish-vars, kaish-version, kaish-vfs |

## Safety rails

- Builtins go through the VFS and see only its mounts — the agent preset
  sandboxes to `$HOME` + `/tmp`, with `/v/` as in-memory scratch under a
  64 MiB per-call budget.
- **External commands resolved via `PATH` run against the real filesystem** —
  the VFS sandbox does not apply to them. Block them at runtime with
  `allow_external_commands=false`, or build without the `subprocess` capability
  feature and they don't exist at all.
- `--overlay` makes a call copy-on-write: writes stay in memory unless the
  script runs `kaish-vfs commit`.
- `set -o latch` (or `KAISH_LATCH=1`) gates destructive commands behind
  nonce confirmation — the command returns exit 2 and a re-run hint instead
  of acting.
- `set -o trash` (or `KAISH_TRASH=1`) diverts deletes to the freedesktop.org
  Trash instead of removing them.

Latch and trash semantics are covered in [docs/LANGUAGE.md](docs/LANGUAGE.md);
the embedder-facing contract (`LatchRequest`, nonce stores, `Kernel::confirm`)
in [docs/EMBEDDING.md](docs/EMBEDDING.md).

## Why 会sh (kaish)?

会sh was originally prototyped as part of 会術 Kaijutsu and was separate enough
it made sense to split it out. Amy was also a fan of ksh and pdksh back in the 00s
so k-ai-sh seems fun.

## Building from Source

```sh
git clone https://github.com/tobert/kaish
cd kaish
cargo build --release
cargo test --all
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
