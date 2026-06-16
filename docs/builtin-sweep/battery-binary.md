# Builtin sweep — encode/binary battery (base64 / xxd / checksum / dd)

Fixed cells for the 3-vendor panel. `base64`/`xxd` are universal and produce
ASCII (hex/base64), so the panel predicts exact bytes. `checksum` is
kaish-specific and value-unpredictable — predict the **output format/shape**,
not the digest (the value is verified by round-trip in Phase 2). `dd` is a
binary-aware subset; all raw-byte output routes through `xxd -p` so the harness
stays text-safe. Temporary; delete with the sweep.

## universal — predict exact bytes from memory

| ID | Task |
|----|------|
| B1 | `printf 'hi' \| base64` |
| B2 | `printf 'aGk=' \| base64 -d` |
| B3 | `printf 'hello' \| base64` |
| B4 | `printf 'hi' \| base64 \| base64 -d` (round-trip) |
| B5 | encode 60 bytes (>57): does output wrap at 76 cols (GNU default) or not? |
| X1 | `printf 'hi' \| xxd` (exact default-dump layout) |
| X2 | `printf 'hi' \| xxd -p` |
| X3 | `printf '6869\n' \| xxd -r -p` |
| X4 | `printf 'hi' \| xxd -p \| xxd -r -p` (round-trip) |

## kaish-specific — predict format/behavior, flag surprise

**`checksum` contract:** compute/verify hashes; default SHA-256; `-a md5|sha1`,
`-c <file>` verify, hashes stdin when piped. **Do NOT reproduce the digest** —
predict only the output SHAPE and trailing newline.

| ID | Input | Task / question |
|----|-------|-----------------|
| CK1 | `printf 'hi' \| checksum` | what does one line of output look like (shape: bare hex? `hash  -`? `hash  filename`?) and how long is the hex? |
| CK2 | `printf 'hi' \| checksum -a md5` | shape + hex length for MD5? |
| CK3 | `sha256sum file.txt` | **GNU namesake.** kaish's tool is `checksum`, not `sha256sum`. What happens when an agent reflexively types `sha256sum`? Loud or silent? |

**`dd` contract:** subset — operands `if= of= bs= count= skip=` as `key=value`
words. **`if=` is required; dd does NOT read stdin.** No `of=` → the bytes are
the result. Bad/unknown operand → error.

| ID | Input | Task / question |
|----|-------|-----------------|
| D1 | `dd if=/dev/zero bs=1 count=3 2>/dev/null \| xxd -p` | output? |
| D2 | `dd if=/dev/zero bs=4 count=1 2>/dev/null \| xxd -p` | output? |
| D3 | `printf 'abc' \| dd bs=1 count=3` | **GNU reflex: dd reads stdin.** kaish requires `if=`. Loud or silent? |
| D4 | `dd if=/dev/zero bogus` | unknown operand — loud or silent? |
