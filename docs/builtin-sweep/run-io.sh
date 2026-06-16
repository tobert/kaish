#!/usr/bin/env bash
# Phase-2 runner: passthrough/io (cat/tee). DEFERRED until the tree builds clean.
# Classifies kaish against expectations-io.md. No GNU. Temporary; delete w/ sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

FX=$(mktemp -d)
trap 'rm -rf "$FX"' EXIT
printf 'apple\nbanana\ncherry\n' > "$FX/A"
printf 'apple\nbanana\ncherry\n' > "$FX/C"

run 'CT1 cat A'        '' "cat $FX/A"        $'apple\nbanana\ncherry\n'
run 'CT3 cat A C'      '' "cat $FX/A $FX/C"  $'apple\nbanana\ncherry\napple\nbanana\ncherry\n'
run 'CT4 echo|cat'     '' 'echo hi | cat'    $'hi\n'
# CT2 cat -n: field-width contested (consensus %6d + tab). Pin once observed:
#   run 'CT2 cat -n' '' "cat -n $FX/A" $'     1\tapple\n     2\tbanana\n     3\tcherry\n'
# CT5 cat /dev/zero: kaish should LOUD-ERROR, but if it HANGS this would wedge the
#   suite — run guarded, manually: `timeout 5 ./target/debug/kaish -c 'cat /dev/zero'`
#   expect: non-zero exit + error message; FAIL if it hangs (timeout) or streams NUL.

# tee — stdout passthrough + file content
run 'TE1 tee stdout' $'hi\n' "tee $FX/out.txt" $'hi\n'
run 'TE1b tee wrote file' '' "cat $FX/out.txt" $'hi\n'
printf 'x\n' > "$FX/ap.txt"
run 'TE2 tee -a stdout' $'a\n' "tee -a $FX/ap.txt" $'a\n'
run 'TE2b tee -a appended' '' "cat $FX/ap.txt" $'x\na\n'

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s)"
exit "$fails"
