# Binary Data — Design Doc

Status: **Phases 1–3 landed + binary plumbed end to end.** value/carrier/boundary;
`dd` + `/dev/urandom`; the data movers; text tools loud-error on binary (no lossy
decode); external commands keep binary intact (capture → Bytes, stdin forwarded
raw). Binary survives pipes, redirects, and external I/O:
`dd if=/dev/urandom | base64 | base64 -d | wc -c` == 16; `curl url > file.bin`;
`printf … | base64`. **Effort essentially complete.** Decision 2026-06-13: a
generic `encode`/`decode` was **dropped** — `base64` and `xxd` (`-p`/`-r`) already
bridge text↔bytes, so a generic pair would only duplicate them and invite a
basenc-style "tons of formats × weird flags" surface that's easy for an agent to
misuse. The one real gap is URL/percent encoding; add a focused
`urlencode`/`urldecode` only if web work needs it. GH #176 (2026-07-17) closed
the last two residuals: `ExecContext::stdin`/`ExecuteOptions::stdin` are
bytes-typed, so a `< binfile` redirect (or an embedder's pre-read
`with_stdin(Vec<u8>)`) feeds binary through intact rather than erroring at
redirect setup; `wc -m`/`-w`/default now refuse invalid UTF-8 loudly instead of
lossy-decoding it (see below). A heredoc/here-string body interpolating a
binary value is unaffected and unchanged — that's text-sink territory
(`value_to_text_sink`), correctly loud already, not a stdin-typing residual.

### Producer coercion is content-sniffing — an accepted tradeoff

`success_text_or_bytes` (used by `cat`/`head -c`/`base64 -d`/`xxd -r`/`tee`) returns
**Text when the bytes are valid UTF-8, Bytes otherwise**. This is content-driven
typing: the *same* command yields a text result on text input and a `Bytes` result
on binary, so a tool's `--json` shape varies with content (`"out":"hi"` vs
`{"_type":"bytes",…}`). We accept this (flagged in DeepSeek review): the alternative
— always-`Bytes` — forces every `cat`/`head` through the binary envelope and breaks
the common text path. It differs from the banned JSON *value* sniffing: content is
never mutated, only its text-vs-binary tag is detected losslessly. Known minor
edge: `cat` multi-file / `-n` still rejects binary with `invalid UTF-8`
(single-file is byte-aware). `wc -m`/`-w`/default used to over-count binary via
`U+FFFD` expansion; fixed in GH #176 (2026-07-17) to refuse invalid UTF-8
loudly instead — `-c`/`-l` are pure byte-level counts and are unaffected.
Author: design notes from a 2026-06-13 session (out of the synthetic-`/dev` work)
Related: [LANGUAGE.md](LANGUAGE.md), [issues.md](issues.md), `docs/help/vfs.md`,
`project_dev_fs.md` (auto-memory), `arch_no_json_sniffing.md` (auto-memory)
Review: DeepSeek V4-Pro adversarial pass 2026-06-13 — folded in the `if=`
keyword conflict (blocker), `OutputPayload` enum, `Value::Blob` overlap, the
auto-coerce hazards/scope, the expanded test suite, and the `dd` caps. Its BOM
claim was checked and rejected (a BOM is valid UTF-8 and coerces).

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

### Hazards of auto-coerce, and where it's scoped

The rule is right for *file/device reads producing a value*, but it has sharp
edges that must be documented and that argue for **localizing coercion to the
consumer that wants text, not the carrier**:

- **Multibyte split (real behavior change).** `head -c 1` / `dd bs=1 count=1`
  over a UTF-8 file can slice mid-codepoint → invalid UTF-8 → loud error, where
  today `from_utf8_lossy` yields `U+FFFD`. POSIX `head -c` counts *bytes* and
  doesn't care about encoding; under the rule, a correct byte slice that isn't
  text becomes an error. That's defensible (it *is* binary now) but it is a
  change, and the error message must say so.
- **Binary that happens to be valid UTF-8.** A `.wasm`/key file that decodes by
  chance flows through as text and gets newline-split or substring-matched —
  content preserved, *type* wrong. This differs from the banned JSON sniffing
  (which mutates output) but still burns a consumer that ignores `content_type`.
- **BOM is NOT a problem.** (Corrected from review: `String::from_utf8(EF BB
  BF)` returns `Ok("\u{feff}")` — a BOM is valid UTF-8 and coerces cleanly. The
  resulting string carries a leading `U+FEFF`, exactly as text tools see today.)
- **Migration blast radius.** 10 `from_utf8_lossy` sites in builtins (+ `context.rs`)
  and **26 callers of `read_stdin_to_string`** encode the text assumption. Each
  needs a decision — accept-and-coerce vs. require-bytes — and a `read_stdin_to_bytes`
  sibling. `base64 -d`/`xxd -r`/`checksum` want raw bytes; `jq`/`grep` want text.
  This is per-tool work Phase 2 must enumerate, not a blanket switch.

So the operative rule is two-tier: **reads yield `Bytes`; coercion happens at the
point a tool declares it needs text** (and fails loud there), rather than
silently at the carrier.

## Value model

```
kaish-types:
  Value::Bytes(Vec<u8>)                 // a binary value (vars, $() capture, args)
  OutputData::Bytes { data: Vec<u8> }   // a builtin's binary output

ExecResult:
  enum OutputPayload { Text(String), Bytes(Vec<u8>) }
  out: OutputPayload                    // text XOR bytes, enforced by the type
```

**Use an enum, not sibling fields.** An earlier draft proposed `out: String` +
`out_bytes: Option<Vec<u8>>` as "text XOR bytes" — but two fields admit the
invalid both-set state, and every builder (`set_out`/`push_out`/`clear_out`,
`success`/`failure`/`from_parts`/`with_output`/…) would have to defend the
invariant by hand. `OutputPayload::{Text,Bytes}` makes the invalid state
unrepresentable and puts the coercion in exactly one place:

```rust
fn text_out(&self) -> io::Result<Cow<str>> {
    match &self.out {
        OutputPayload::Text(s) => Ok(Cow::Borrowed(s)),
        OutputPayload::Bytes(b) => match std::str::from_utf8(b) {
            Ok(s) => Ok(Cow::Borrowed(s)),
            Err(_) => Err(/* "binary data — pipe through base64/xxd …" */),
        },
    }
}
```

- `$(cmd)` and assignment **preserve the type**: `key=$(random --bytes 32)`
  holds `Value::Bytes`, not text. `echo $key` is a loud error; `encode base64
  $key` (or `"$(encode base64 $key)"`) is how you get a string.
- `text_out()` becomes fallible. ~200 call sites and the
  `accumulate_result`/sequence-join path must handle the error rather than
  unwrap — joining a bytes stage onto a text stage is itself a loud error, at
  accumulation time. This is the bulk of Phase 1's blast radius; scope it
  honestly.

### One binary value — `Value::Blob` was deleted

An earlier draft proposed keeping the pre-existing `Value::Blob(BlobRef)` (an
out-of-line handle into `/v/blobs`) alongside the new inline `Value::Bytes`,
split by size. Investigation killed that: **`Value::Blob` was never constructed
anywhere** — a vestigial variant defended in ~20 match arms with zero producers,
and `BlobRef` was never used as a type (one stale doc comment aside). Per
"[[no legacy dual-representations]] — delete old code immediately," Phase 1
**removed `Value::Blob` and `BlobRef`** and made `Value::Bytes` the single binary
value type.

Persisting large binary stays a *separate VFS concern*: plain files under
`/v/blobs`, written/read via the client's `write_blob`/`read_blob` (which already
work on `String` ids and raw bytes, never `BlobRef`). If a future spill-to-store
path is wanted, it spills `Value::Bytes` → a `/v/blobs` file + path, not a second
`Value` variant.

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

## Conversions — the text↔bytes bridges

**Decision 2026-06-13: no generic `encode`/`decode`.** The bridges already exist
as byte-aware builtins and that's enough:
- **`base64`** — bytes → base64 text (encode) and base64 text → bytes (`-d`).
- **`xxd`** — bytes → hex (`-p`) and hex → bytes (`-r`).

A generic `encode <fmt>`/`decode <fmt>` would only duplicate those and pull in a
`basenc`-style matrix of formats × formatting flags (`--wrap`/`--ignore-garbage`/
base64url/no-pad) — surface that's easy for an agent to pick wrong, for little
gain. The one genuine gap is URL/percent encoding (query strings, `%20` decode);
if web work needs it, add a small flagless `urlencode`/`urldecode`, not a
framework. Revisit a unified pair only if the format count ever justifies it.

## `dd` — the binary mover

A small, classic `dd` is the natural binary-aware copy tool and the north-star
integration target (below). Operands use the traditional `key=value` form.

> **BLOCKER — `if=` collides with the `if` keyword.** My earlier "lexes as
> barewords (awk precedent)" claim was **wrong**: `awk.rs:128` splits an
> *already-parsed positional string* at runtime; the lexer never sees a keyword.
> But `if` is `Token::If`, and `keyword_as_bareword` (`parser.rs:1940-1954`)
> lists only the *closing* keywords (`done`/`fi`/`then`/`else`/`elif`/`in`/`do`/
> `esac`/`set`) — **not `If`**. So `dd if=x` parses `dd` as the command and then
> tries to start an `if`-statement on `if=x`. (`of=`/`bs=`/`count=`/`skip=` are
> plain idents and are fine; the `/` in `if=/dev/urandom` is also fine — the key
> alone is the problem.) This must be fixed in the parser before `dd` ships:
> - **(preferred)** make `keyword_as_bareword` *context-sensitive* — in argument
>   position (after a command name) accept statement-openers (`If`/`For`/`While`/
>   `Case`) as barewords too; or
> - emit a dedicated `WordAssign{key,value}` token for `key=value` whose key may
>   be a keyword; or
> - **(fallback)** use `--if=FILE` flags and drop the classic idiom.
> Gates Phase 3.

Supported operands (80% subset):

| operand | meaning | default |
|---------|---------|---------|
| `if=FILE`  | input file | stdin |
| `of=FILE`  | output file | stdout (as a `Bytes` result) |
| `bs=N`     | block size in bytes | 512 |
| `count=N`  | number of blocks to copy | until EOF |
| `skip=N`   | skip N input blocks before copying | 0 |

- Total bytes = `bs * count`. `dd` reads via the `Filesystem::read_range`
  byte-count plumbing added for `/dev/zero` (`ReadRange::bytes(skip*bs,
  count*bs)`), so it composes with endless devices without hanging.
- `dd` is inherently binary: it carries `Bytes`, never coerces, and writes via
  the backend (`of=`) or emits a `Bytes` result (no `of=`).
- A short status line (`N bytes copied`) goes to stderr, matching `dd`.
- **Total-bytes cap, independent of the device cap.** DevFs caps a *device*
  read at 64 MiB (`dev.rs`), but `dd if=/tmp/huge bs=1M count=1000000` reads a
  *real* file and would allocate the whole request. `dd` needs its own cap on
  `bs*count` (loud error above it) so a fat `count` can't OOM the kernel —
  whether or not the source is a device. Also: `bs=1M count=128` (128 MiB)
  trips the 64 MiB *device* cap; either raise/justify that cap or have `dd`
  chunk its reads to stay under it.
- **Streaming vs. buffering.** The all-at-once `read_range(bs*count)` buffers
  the whole transfer in RAM. Faithful `dd` reads a block, writes a block,
  repeats — bounded memory regardless of `count`. Decide: stream block-at-a-time
  (preferred for large transfers; also the natural cap mechanism) or buffer.
- **`skip=` on a non-seekable device consumes entropy.** For `/dev/urandom`,
  `skip=N` means generating and discarding `N*bs` bytes (there's no seek). Note
  it; it's correct but not free.

## Chokepoints to remove (the actual work)

1. File read path: stop `from_utf8` in `head` / `cat` / `<`-redirect /
   heredoc; carry `Vec<u8>`, decode only when text is required.
2. `ExecResult` / `OutputData`: `OutputPayload` enum + the coercion rule.
3. Pipe *consumption*, not transport. The pipe ring buffer is **already
   `VecDeque<u8>`** (`pipe_stream.rs:37`) — byte-clean today. The text
   assumption lives only at the ends: `pipeline.rs` calls `text_out()` then
   `.as_bytes()` to fill the pipe, and `read_stdin_to_string` (26 callers)
   pulls it back as a `String`. The work is a `bytes_out()` write path + a
   `read_stdin_to_bytes` sibling + per-tool opt-in — not a transport rewrite.
4. `encode`/`decode` builtins + realigned `base64` + the loud-error guard at
   text sinks (`echo`, interpolation, MCP/REPL text).
5. **`for x in $(cmd)` over a `Bytes` value.** The `$()` capture (`kernel.rs`)
   returns `Value::String` today via `text_out()`; preserving the type means it
   can return `Value::Bytes`. Iterating bytes as lines is meaningless — define
   it as a **loud error** ("binary value is not iterable; `decode`/`encode`
   first"), not a silent byte-by-byte loop.

## Phased plan (each phase independently shippable)

- **Phase 1 — value + boundary. ✅ DONE.** `Value::Bytes` (single binary type;
  `Value::Blob`/`BlobRef` deleted), the `ExecResult.out` `OutputPayload::{Text,
  Bytes}` enum (wire-compatible serde), `text_out()` infallible + `try_text_out()`
  loud guard + `out_bytes()`/`is_bytes()`/`success_bytes()`, base64 envelope +
  `hex_dump` helpers, and boundary rendering (REPL hex dump via `format_output`,
  `--json`/MCP base64 envelope via `value_to_json`). `OutputData::Bytes` deferred
  to Phase 2, where the pipe/consumption rework makes its text-shaped methods
  meaningful — until then a binary result is produced via `ExecResult::success_bytes`.
  No builtin produces bytes yet, so nothing changes for users.
- **Phase 2 — transit. (in progress)** First producer landed: `cat` of a single
  non-UTF-8 file yields a `Bytes` result (terminal hex dump + `--json` base64
  envelope, via `apply_output_format`'s bytes branch) instead of erroring —
  multi-file/`-n` stay text. Remaining: byte-clean pipe *consumption*
  (`read_stdin_to_bytes`, a `bytes_out()` write path so `cat blob | xxd` carries
  bytes intact), `head`/`<`-redirect producers, and `OutputData::Bytes`. Note:
  there's no way to *write* a non-UTF-8 file through kaish until Phase 3's `dd`,
  so end-to-end binary tests through `kernel.execute` wait on a writer.
- **Phase 3 — tools + devices. ✅ dd + urandom DONE.** `dd` builtin
  (`if=`/`of=`/`bs=`/`count=`/`skip=`, k/M/G suffixes, 256 MiB transfer cap, status
  to stderr; reads via `read_range`, writes via the backend, or emits a `Bytes`
  result with no `of=`). `/dev/urandom` + `/dev/random` added to `DevFs` (OS
  CSPRNG via `getrandom`). The `if=` parser blocker is fixed (keyword tokens as
  `key=` keys). North-star test green in `sandbox_mode_tests`. Still to do:
  `encode`/`decode` builtins, realign `base64`, a `random` builtin.
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

A compact happy-path test is a false economy — it wouldn't catch the failure
modes the design is *for*. Expand to a suite that also bites:

```sh
dd if=/dev/urandom bs=1024 count=10 | dd of=/dev/null        # bytes survive a PIPE stage
dd if=/dev/urandom bs=16 count=1 | grep x                    # MUST fail loud (not search garbage)
x=$(dd if=/dev/urandom bs=1 count=1); echo $x                # MUST error (bytes can't echo)
dd if=/dev/zero bs=8 count=1                                 # result is Bytes([0;8]) — check via --json/kaish-last
dd if=/dev/urandom of=/tmp/x bs=1 count=1; cat /tmp/x | encode hex   # binary→hex exact, lossless
dd if=/dev/urandom of=/dev/null bs=1024 count=10 2>&1 | grep "10240" # stderr status line present
```

Home: a NoLocal kernel-routed test (`KernelConfig::isolated()`) beside today's
DevFs tests in `tests/sandbox_mode_tests.rs`.

## Open questions

- **Hex-dump cap & encoded-output truncation.** Default bytes-before-truncation
  for the REPL dump and the base64 envelope (tie to the output-limit default?).
  base64 expands ~33%, so a 64 MiB payload → ~85 MiB of JSON and likely blows
  past MCP message limits. Decide: truncate the **raw bytes before encoding**
  (and report `len` so it's never silent), not the encoded string. Spilling a
  large `Bytes` to a `Blob` (see value model) is the cleaner answer for MCP.
- **`--json` is a breaking change for consumers.** A top-level `{"type":"bytes",
  "encoding":"base64","data":…}` envelope is a new shape any existing `--json`
  parser must learn. Call it out in release notes.
- **`Bytes` in arithmetic / `[[ ]]`.** An error (no implicit numeric/string
  coercion); confirm the message names the fix (`decode`/`encode`).
- **`random` surface.** `random --bytes N [--hex|--base64]` returns text;
  `random --bytes N` alone returns `Bytes`. Bikeshed `--int`/range later.
- **`getrandom` platform behavior.** Linux `getrandom(2)` blocks until the pool
  is seeded (fine); WASI/older-macOS quality and the existing entropy-failure
  message gap (`issues.md` mktemp note) should be documented for `/dev/urandom`.
