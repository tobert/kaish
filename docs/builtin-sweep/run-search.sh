#!/usr/bin/env bash
# Phase-2 runner: search (grep/rg). DEFERRED until the tree builds clean.
# Classifies kaish against banked consensus (expectations-search.md). No GNU
# oracle. Temporary; delete with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

SG=$'foo\nFoobar\nbaz\nfoofoo\n'
SB=$'aa\na\n'

run 'G1 grep foo'        "$SG" 'grep foo'          $'foo\nfoofoo\n'        0
run 'G2 grep -v foo'     "$SG" 'grep -v foo'       $'Foobar\nbaz\n'        0
run 'G3 grep -i foo'     "$SG" 'grep -i foo'       $'foo\nFoobar\nfoofoo\n' 0
run 'G4 grep -c foo'     "$SG" 'grep -c foo'       $'2\n'                  0
run 'G5 grep -o foo'     "$SG" 'grep -o foo'       $'foo\nfoo\nfoo\n'      0
run 'G6 grep -n foo'     "$SG" 'grep -n foo'       $'1:foo\n4:foofoo\n'    0
run 'G7 grep -E baz|Foo' "$SG" "grep -E 'baz|Foo'" $'Foobar\nbaz\n'        0
run 'G8 grep zzz (no-match rc=1)' "$SG" 'grep zzz' ''                      1
run 'G9a grep -E a{2} (ERE)'      "$SB" "grep -E 'a{2}'" $'aa\n'           0

# G9b — DeepSeek's BRE reflex MUST fail loud (sed-consistent), not silently
# literal-match or no-match. kaish's error rc is TBD; verify manually that it
# errors with an ERE hint, then pin the rc here:
# run 'G9b grep BRE a\{2\} (expect LOUD)' "$SB" "grep 'a\{2\}'" '' <err-rc>

run 'R1 rg foo (bare lines, no -n)' "$SG" 'rg foo'    $'foo\nfoofoo\n'        0
run 'R2 rg -i foo'                  "$SG" 'rg -i foo' $'foo\nFoobar\nfoofoo\n' 0
run 'R3 rg -o foo'                  "$SG" 'rg -o foo' $'foo\nfoo\nfoo\n'      0
run 'R4 rg -c foo'                  "$SG" 'rg -c foo' $'2\n'                  0
run 'R5 rg -n foo'                  "$SG" 'rg -n foo' $'1:foo\n4:foofoo\n'    0
run 'R6 rg zzz (no-match rc=1)'     "$SG" 'rg zzz'    ''                      1

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s) — see above"
exit "$fails"
