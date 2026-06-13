# Binary Data — Design Doc

Status: **proposal / design exploration** (not yet implemented)
Author: design notes from a 2026-06-13 session (out of the synthetic-`/dev` work)
Related: [LANGUAGE.md](LANGUAGE.md), [issues.md](issues.md), `docs/help/vfs.md`,
`project_dev_fs.md` (auto-memory), `arch_no_json_sniffing.md` (auto-memory)

## Motivation

kaish is UTF-8 text end to end: `ExecResult.out` is a `String`, `OutputData`'s
variants are all string-shaped, and the inter-command pipe is consumed as text
(`read_stdin_to_string`, line-oriented `BufReader`). That is fine for the 80%
shell job, but it has hard edges:

- File reads stringify. `head`, `cat`, and the `<` / heredoc redirect paths all
  do `String::from_utf8(...)` on bytes the backend already handed up as
  `Vec<u8>` — so a binary file is an error at *read*, and a lossy decode would
  corrupt the content irreversibly.
- There is nowhere to *put* bytes. Even a byte-clean tool (`checksum`,
  `base64 -d`) has no value type to carry raw bytes to the next stage.
- Consequently `/dev/urandom` cannot exist (see `dev.rs`): random bytes are
  ~never valid UTF-8, and the streaming idioms (`cat /dev/urandom | tr | head`)
  need infinite byte streams kaish doesn't have.

This doc proposes a **first-class binary value** so bytes can survive transit
(pipes + file I/O) and be encoded explicitly at the boundary where a human or
an agent actually consumes them.

## The reframe: binary is an encoding/boundary problem

An LLM — kaish's primary audience — cannot consume raw bytes. Anything that
reaches the agent, a terminal, or an MCP response must be text. So "support
binary" is two narrow things, not a general binary runtime:

1. **Transit** — let bytes flow through pipes and file I/O without a UTF-8
   chokepoint mangling them.
2. **Boundary encoding** — when bytes surface to a human/agent, render them as
   hex/base64 (or a hex dump), *explicitly*, never by silent stringification.

## Prior art: nushell

nushell is the closest analog — a structured, typed shell with a first-class
`binary` type. It flows through pipes, renders as a **hex dump** on display, and
every text↔bytes conversion is explicit (`into binary`, `decode utf-8`,
`encode base64`). Nothing silently stringifies. That discipline matches kaish's
existing stances (no word splitting, no JSON sniffing, fail loud) and is the
model adopted here.

## Design principle: one typed value, coerced only when lossless

Add a `Bytes` value and **one rule** that governs every text context:

> **Bytes coerce to text iff they are valid UTF-8; otherwise it is a loud
> error.** Coercion is lossless-or-error — never a `from_utf8_lossy` mash.

This is what keeps the change small rather than sprawling:

- File reads yield `Bytes`. Valid-UTF-8 content coerces transparently, so
  `cat file.txt | grep x` is untouched. This is *detection*, not heuristic
  typing — distinct from the JSON sniffing we've banned (`arch_no_json_sniffing`).
- `cat image.png` → hex dump at the boundary; `cat image.png | grep` → grep
  asks for text → coercion fails → `error: invalid UTF-8, not text — pipe
  through xxd/base64 or redirect to a file`.
- Binary-aware tools (`base64`, `checksum`, `dd`, `gzip`) take the raw bytes and
  never coerce.

The stricter alternative — *always* require an explicit `decode utf-8` — is
purer but breaks every existing text pipeline and taxes the 99% case. Rejected.

## Value model

```
kaish-types:
  Value::Bytes(Vec<u8>)             // a binary value (vars, $() capture, args)
  OutputData::Bytes { data: Vec<u8> }   // a builtin's binary output

ExecResult:
  out: String                       // text result (unchanged)
  out_bytes: Option<Vec<u8>>        // binary result; a result is text XOR bytes
```

- `$(cmd)` and assignment **preserve the type**: `key=$(random --bytes 32)`
  holds `Value::Bytes`, not text. `echo $key` is a loud error; `encode base64
  $key` (or `"$(encode base64 $key)"`) is how you get a string.
- `text_out()` on a bytes result applies the coercion rule (UTF-8 → `Cow::Owned`,
  else error). All current `text_out()` callers route through it, so they get
  the guard for free.

## Boundary rendering — display, don't coerce

A *top-level* bytes result is displayed (not coerced):

- **REPL:** a hex dump (`xxd`-style: offset, hex columns, ASCII gutter).
- **`--json` / MCP:** structured and self-describing —
  `{"type":"bytes","encoding":"base64","data":"…","len":N}` — which is what an
  agent can actually act on.
- **Truncation cap:** large blobs reuse the existing output-limit /
  `SpillMode::Memory` machinery and exit-3 signal (`arch_spill_mode_memory`);
  the hex dump and the base64 envelope both honor the cap and report `len` so
  truncation is never silent.

## Conversions — the only text↔bytes bridges

- `encode <hex|base64> [input]` — bytes → text.
- `decode <hex|base64|utf-8> [input]` — text → bytes (`utf-8` validates text → bytes).
- The existing `base64` builtin is realigned to produce/consume `Bytes` rather
  than smuggling binary through strings.

## `dd` — the binary mover

A small, classic `dd` is the natural binary-aware copy tool and the north-star
integration target (below). Operands use the traditional `key=value` form —
which already lexes as bareword args (precedent: `awk prog var=value file`,
`awk.rs:128`); a spot-check is owed for slashed values like `if=/dev/urandom`.

Supported operands (80% subset):

| operand | meaning | default |
|---------|---------|---------|
| `if=FILE`  | input file | stdin |
| `of=FILE`  | output file | stdout (as a `Bytes` result) |
| `bs=N`     | block size in bytes | 512 |
| `count=N`  | number of blocks to copy | until EOF |
| `skip=N`   | skip N input blocks before copying | 0 |

- Total bytes = `bs * count`. `dd` reads exactly that many via the
  `Filesystem::read_range` byte-count plumbing already added for `/dev/zero`
  (`ReadRange::bytes(skip*bs, count*bs)`), so it composes with endless devices
  without hanging.
- `dd` is inherently binary: it carries `Bytes`, never coerces, and writes via
  the backend (`of=`) or emits a `Bytes` result (no `of=`).
- A short status line (`N bytes copied`) goes to stderr, matching `dd`.

## Chokepoints to remove (the actual work)

1. File read path: stop `from_utf8` in `head` / `cat` / `<`-redirect /
   heredoc; carry `Vec<u8>`, decode only when text is required.
2. `ExecResult` / `OutputData`: add the bytes carrier + the coercion rule.
3. Pipe ends: byte-clean rather than `String`-clean (the channel can hold
   `Vec<u8>`; line tools split on `\n` over bytes).
4. `encode`/`decode` builtins + realigned `base64` + the loud-error guard at
   text sinks (`echo`, interpolation, MCP/REPL text).

## Phased plan (each phase independently shippable)

- **Phase 1 — value + boundary.** `Value::Bytes` / `OutputData::Bytes`, the
  `ExecResult` carrier, the coercion rule, and boundary rendering (REPL hex
  dump, `--json`/MCP structured base64). No tool behavior changes yet.
- **Phase 2 — transit.** Byte-clean pipe ends; kill the file-read chokepoints so
  `head`/`cat`/`<` carry bytes and only decode on demand.
- **Phase 3 — tools + devices.** `encode`/`decode`, realign `base64`, add the
  `random` builtin and `dd`, and drop **`/dev/urandom` + `/dev/random`** into
  `DevFs` (byte-count plumbing already exists; `getrandom` is already a dep).
- **Phase 4 — as demand appears.** `gzip`/`gunzip`, image/blob helpers, etc.

## North-star acceptance test

The end-to-end proof that the path is real (lands with Phase 3):

```sh
# random bytes from an endless device, through dd, into the sink
dd if=/dev/urandom of=/dev/null bs=1024 count=10        # 10240 bytes copied, exit 0

# and into a real file, then verify the size and that it's non-trivial
dd if=/dev/urandom of=/tmp/rand.bin bs=1024 count=10
[[ $(wc -c < /tmp/rand.bin) == 10240 ]]                 # exact byte count
# entropy sanity: two draws differ
dd if=/dev/urandom of=/tmp/a.bin bs=16 count=1
dd if=/dev/urandom of=/tmp/b.bin bs=16 count=1
[[ "$(checksum --sha256 /tmp/a.bin)" != "$(checksum --sha256 /tmp/b.bin)" ]]
```

Asserts, in order: counted reads of an endless device terminate at exactly
`bs*count`; binary survives `dd` → file write intact; the byte count is exact
(no UTF-8 mangling); and the source is actually random (two draws diverge).
A NoLocal-mode kernel-routed test (`KernelConfig::isolated()`) is the home for
it, alongside today's DevFs tests in `tests/sandbox_mode_tests.rs`.

## Open questions

- **Hex-dump cap.** Default bytes-before-truncation for the REPL dump and the
  base64 envelope (tie to the existing output-limit default?).
- **`dd` operand lexing.** Confirm `if=/dev/urandom` (with `/`) stays a single
  bareword arg; if not, a small lexer tweak rather than `--if=` flags, to keep
  the recognizable idiom.
- **`Bytes` in arithmetic / `[[ ]]`.** Almost certainly an error (no implicit
  numeric/string coercion); confirm the message names the fix.
- **`random` surface.** `random --bytes N [--hex|--base64]` returns text;
  `random --bytes N` alone returns `Bytes`. Bikeshed `--int`/range later.
