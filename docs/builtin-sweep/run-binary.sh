#!/usr/bin/env bash
# Phase-2 runner: encode/binary (base64/xxd/checksum/dd). DEFERRED until the tree
# builds clean. Classifies kaish against expectations-binary.md. No GNU. All
# raw-byte output routes through xxd -p / base64 so comparison stays text-safe.
# Temporary; delete with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

# base64 — values unanimous; decode drops trailing newline (sentinel-checked)
run 'B1 base64 hi'        ''       "printf 'hi' | base64"             $'aGk=\n'
run 'B2 base64 -d'        ''       "printf 'aGk=' | base64 -d"        'hi'
run 'B3 base64 hello'     ''       "printf 'hello' | base64"          $'aGVsbG8=\n'
run 'B4 base64 round-trip' ''      "printf 'hi' | base64 | base64 -d" 'hi'
# B5 wrap-at-76 is a DOC decision — fill once kaish's wrap default is chosen.

# xxd — -p/-r round-trips clean; default-dump layout (X1) is contested, see doc
run 'X2 xxd -p'           ''       "printf 'hi' | xxd -p"             $'6869\n'
run 'X3 xxd -r -p'        ''       "printf '6869\n' | xxd -r -p"      'hi'
run 'X4 xxd -p round-trip' ''      "printf 'hi' | xxd -p | xxd -r -p" 'hi'
# X1 default dump: pin to canonical xxd layout after observing kaish's spacing:
# run 'X1 xxd default'  '' "printf 'hi' | xxd" $'00000000: 6869                                     hi\n'

# checksum — SHAPE contested (CK1/CK2): observe kaish's line, then assert + a
# compute->verify round-trip. CK3 collision must be loud.
# run 'CK1 checksum sha256 shape' '' "printf 'hi' | checksum" $'<hash>  -\n'
# CK3: sha256sum is not a kaish builtin -> not-found/loud (rc capability-dependent)

# dd — deterministic zero-fill; collisions D3/D4 must be loud (exit 2)
run 'D1 dd zero bs=1 count=3' '' 'dd if=/dev/zero bs=1 count=3 2>/dev/null | xxd -p' $'000000\n'
run 'D2 dd zero bs=4 count=1' '' 'dd if=/dev/zero bs=4 count=1 2>/dev/null | xxd -p' $'00000000\n'
run 'D3 printf|dd (no if=, COLLISION LOUD)' $'abc' 'dd bs=1 count=3' '' 2
run 'D4 dd bogus operand (LOUD)'            ''     'dd if=/dev/zero bogus' '' 2

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s)"
exit "$fails"
