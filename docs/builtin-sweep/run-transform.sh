#!/usr/bin/env bash
# Phase-2 runner: generate/transform (printf/seq/split/jq). DEFERRED until the
# tree builds clean. Classifies kaish against expectations-transform.md. No GNU.
# Temporary; delete with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

# printf (cycling + default-fill are the sharp ones)
run 'P1 printf %s a b c'   '' "printf '%s\n' a b c"   $'a\nb\nc\n'
run 'P2 printf %s-%s x4'   '' "printf '%s-%s\n' a b c d" $'a-b\nc-d\n'
run 'P3 printf %d-%d 1 2 3 (fill 0)' '' "printf '%d-%d\n' 1 2 3" $'1-2\n3-0\n'
run 'P4 printf %05.2f'     '' "printf '%05.2f\n' 3.14159" $'03.14\n'
run 'P5 printf %x 255'     '' "printf '%x\n' 255" $'ff\n'
run 'P6 printf a\\tb'      '' "printf 'a\tb\n'" $'a\tb\n'
run 'P7 printf %s (no arg)' '' "printf '%s\n'" $'\n'

# seq
run 'S1 seq 3'        '' 'seq 3'        $'1\n2\n3\n'
run 'S2 seq 2 5'      '' 'seq 2 5'      $'2\n3\n4\n5\n'
run 'S3 seq 1 2 7'    '' 'seq 1 2 7'    $'1\n3\n5\n7\n'
run 'S4 seq -w 8 10'  '' 'seq -w 8 10'  $'08\n09\n10\n'
run 'S5 seq 3 -1 1'   '' 'seq 3 -1 1'   $'3\n2\n1\n'
run 'S6 seq -s, 1 3'  '' 'seq -s, 1 3'  $'1,2,3\n'
run 'S7 seq 5 2 (empty)' '' 'seq 5 2'   ''

# split — stdin newline-trim, limit, regex trailing-empty, collision
run 'SP1 split : (stdin, trim \n)' $'a:b:c\n' "split ':'"           $'a\nb\nc\n'
run 'SP2 split whitespace'         ''          "split 'hello world'" $'hello\nworld\n'
run 'SP3 split --limit=2'          ''          "split 'a:b:c:d' ':' --limit=2" $'a\nb:c:d\n'
# SP4 trailing-empty is a DECISION (keep vs trim). consensus(2/3)=keep -> blank last line:
run 'SP4 split -r [0-9] (trailing empty?)' '' "split 'a1b2c3' -r '[0-9]'" $'a\nb\nc\n\n'
run 'SP5 split -l 100 (COLLISION, LOUD)'  '' 'split -l 100 access.log' '' 2

# jq — core + the env boundary (must be loud, not silent null)
run 'J1 jq .a'        $'{"a":1,"b":2}\n' "jq '.a'"                 $'1\n'
run 'J2 jq -r .name'  $'{"name":"kaish"}\n' "jq -r '.name'"        $'kaish\n'
run 'J3 jq select'    $'[1,2,3]\n'       "jq '.[] | select(. > 1)'" $'2\n3\n'
run 'J4 jq -c .'      $'{"a":1}\n'       "jq -c '.'"               $'{"a":1}\n'
# J5: contract demands LOUD; rc unknown (jq compile error). Verify NOT silent-null:
# run 'J5 jq env.HOME (expect LOUD, not null)' $'"x"\n' "jq 'env.HOME'" '' <rc>

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s)"
exit "$fails"
