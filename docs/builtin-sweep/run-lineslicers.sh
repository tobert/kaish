#!/usr/bin/env bash
# Phase-2 runner: line-slicers (head/tail/tac/uniq/wc). DEFERRED until the tree
# builds clean. Classifies kaish against banked consensus (expectations-lineslicers.md).
# No GNU oracle. Temporary; delete with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

L4=$'apple\nbanana\ncherry\ndate\n'
LU=$'a\na\nb\na\nc\nc\n'
LW=$'hello world\nfoo\n'
LN=$'a\nb'   # no trailing newline

run 'H1 head -n 2'   "$L4" 'head -n 2'   $'apple\nbanana\n'
run 'H2 head -n -1'  "$L4" 'head -n -1'  $'apple\nbanana\ncherry\n'
run 'H3 head -c 5'   "$L4" 'head -c 5'   'apple'
run 'TL1 tail -n 2'  "$L4" 'tail -n 2'   $'cherry\ndate\n'
run 'TL2 tail -n +2' "$L4" 'tail -n +2'  $'banana\ncherry\ndate\n'
run 'TL3 tail -c 5'  "$L4" 'tail -c 5'   $'date\n'
run 'TC1 tac'        "$L4" 'tac'         $'date\ncherry\nbanana\napple\n'
run 'U1 uniq'        "$LU" 'uniq'        $'a\nb\na\nc\n'
run 'U3 uniq -d'     "$LU" 'uniq -d'     $'a\nc\n'
run 'W5 wc -l (no-final-nl; value=1)' "$LN" 'wc -l' $'1\n'

# value-consensus is unpadded (2/3); these MATCH only if kaish chose unpadded.
# If kaish pads, that's the DOC decision, not a corruption — reclassify, don't panic.
run 'W1 wc -l'  "$LW" 'wc -l' $'2\n'
run 'W2 wc -w'  "$LW" 'wc -w' $'3\n'
run 'W3 wc -c'  "$LW" 'wc -c' $'16\n'

# FORMAT UNDECIDED — fill expected once kaish's field-width rule is chosen + documented:
# run 'W4 wc'      "$LW" 'wc'      $'<lines words bytes, chosen padding>\n'
# run 'U2 uniq -c' "$LU" 'uniq -c' $'<count-prefix, chosen width>\n'

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s) — see above"
exit "$fails"
