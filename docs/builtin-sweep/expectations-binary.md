# Builtin sweep — encode/binary expectations (base64 / xxd / checksum / dd)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. base64/xxd = model-memory; checksum/dd = contract-first. Reference =
consensus, not GNU (`[[model-memory-over-gnu-oracle]]`). All binary output routes
through `xxd -p`/base64 so the harness stays text-safe. Routing: **CODE** / **DOC**.
Legend: ↵ / ∅↵.

## base64 — values unanimous (3/3)

| ID | program | expected | nl |
|----|---------|----------|----|
| B1 | `printf 'hi' \| base64` | `aGk=` | ↵ |
| B2 | `printf 'aGk=' \| base64 -d` | `hi` | **∅↵** |
| B3 | `printf 'hello' \| base64` | `aGVsbG8=` | ↵ |
| B4 | `printf 'hi' \| base64 \| base64 -d` | `hi` | **∅↵** |

- **B5 — line wrapping (DOC).** 3/3: GNU wraps base64 at **76 columns** by default
  (`-w0` disables). If kaish emits one long line instead, that diverges from the
  reflex. Decide kaish's default (wrap-76 vs no-wrap) + whether `-w0` exists, then
  document. Decode (B2/B4) yields **no trailing newline** — same producer-adds /
  transformer-doesn't pattern as wave-1 `tr -cd`.

## xxd — values unanimous, layout contested

| ID | program | expected | nl |
|----|---------|----------|----|
| X2 | `printf 'hi' \| xxd -p` | `6869` | ↵ |
| X3 | `printf '6869\n' \| xxd -r -p` | `hi` | **∅↵** |
| X4 | `printf 'hi' \| xxd -p \| xxd -r -p` | `hi` | **∅↵** |

- **X1 default-dump layout (DOC+CODE, fidelity).** All three produced
  `00000000: 6869 … hi` but **disagreed on the exact inter-column spacing**
  (DeepSeek/Sonnet vs Gemini's "37 spaces"). The canonical xxd line is: 8-hex
  offset, `: `, hex bytes in 2-byte groups padded to a 16-byte (set) field, two
  spaces, ASCII gutter. xxd output is sometimes consumed byte-exact → pin kaish's
  layout to the canonical form with a test and document it. `-p`/`-r -p`
  round-trips are clean (3/3).

## checksum — contract-first; SHAPE contested (DOC)

| ID | program | expected shape | route |
|----|---------|----------------|-------|
| CK1 | `printf 'hi' \| checksum` | 64 hex `<hash>␣␣-` (2/3) vs bare hex (Gemini) | DOC |
| CK2 | `printf 'hi' \| checksum -a md5` | 32 hex, same shape | DOC |
| CK3 | `sha256sum file.txt` | **loud / not-found, rc≠0** | DOC + CODE |

- **CK1/CK2 — output shape (DOC, decide+document).** 2/3 expect the
  `sha256sum`-convention `<hash>␣␣-` (dash = stdin); Gemini expected bare hex.
  kaish's actual shape is unknown → Phase 2 observes it. The choice matters for
  **`checksum -c` round-trip compatibility** (verify-mode must parse what
  compute-mode emits) — assert that round-trip in Phase 2, then document the line
  format. Don't predict the digest; verify by round-trip.
- **CK3 — NAMESAKE COLLISION.** `sha256sum`/`md5sum` are not kaish builtins. 3/3:
  loud (command-not-found, rc≠0) in a no-subprocess kernel; with `subprocess` it
  would run the host tool (Sonnet's nuance — capability-dependent, and itself a
  consistency wrinkle). CODE: confirm it's not silent. DOC: the idiom is
  `checksum`, cross-reference from any sha256sum muscle-memory.

## dd — contract-first subset

| ID | program | expected | route |
|----|---------|----------|-------|
| D1 | `dd if=/dev/zero bs=1 count=3 2>/dev/null \| xxd -p` | `000000` | — (3/3) |
| D2 | `dd if=/dev/zero bs=4 count=1 2>/dev/null \| xxd -p` | `00000000` | — (3/3) |
| D3 | `printf 'abc' \| dd bs=1 count=3` | *(empty)*, **rc 2 LOUD** | CODE+DOC (3/3) |
| D4 | `dd if=/dev/zero bogus` | *(empty)*, **rc 2 LOUD** | CODE (3/3) |

- **D3 — NAMESAKE COLLISION (the big dd surprise).** kaish `dd` requires `if=` and
  does **not** read stdin; POSIX dd defaults to stdin. 3/3: must fail loud
  (`dd: if= is required`), never silently drop the piped data. CODE: confirm exit
  2 + hint. DOC prominently — this is a real divergence from muscle memory.
- **D4** unknown operand → exit 2 loud (3/3), matches source.

## Wave-2 cross-tool levers
1. **NAMESAKE COLLISION must be loud** (SP5, CK3, D3, D4 — all 3/3). The wave-2
   headline. Phase 2: assert rc≠0 + a hint, never silent.
2. **jq env (J5)** — contract says loud; verify jaq doesn't silently return
   `null` (P1-class).
3. **Format/layout decisions (DOC):** base64 wrap-76, xxd dump spacing, checksum
   line shape. Decide, pin, document — same class as wave-1 wc/uniq padding.
4. **Decode/transform drop trailing newline** (base64 -d, xxd -r) — sentinel-checked.
