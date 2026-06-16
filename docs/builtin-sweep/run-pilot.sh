#!/usr/bin/env bash
# Phase-2 runner for the pilot battery: classify kaish against the banked
# model-consensus expectations (expectations-pilot.md). DEFERRED until the awk
# session settles and the tree builds clean. No GNU oracle is used.
#
#   cargo build -p kaish-repl && docs/builtin-sweep/run-pilot.sh
#
# Exit nonzero if any cell is SILENT-WRONG (the data hazard). Temporary; delete
# with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

# --- cut ---
run 'C1 cut -d: -f7'          $'root:x:0:0:root:/root:/bin/bash\n' 'cut -d: -f7'                          $'/bin/bash\n'
run 'C2 cut -d: -f1,3'        $'root:x:0:0:root:/root:/bin/bash\n' 'cut -d: -f1,3'                        $'root:0\n'
run 'C3 cut -d, -f3'          $'alice,30,nyc\nbob,25,la\n'         'cut -d, -f3'                          $'nyc\nla\n'
run 'C4 cut -d, -f2-'         $'alice,30,nyc\nbob,25,la\n'         'cut -d, -f2-'                         $'30,nyc\n25,la\n'
run 'C5 cut -c1-5'            $'hello world\n'                     'cut -c1-5'                            $'hello\n'
run 'C6 cut -d, -f2 (no-delim, FLAG)' $'nodelim\n'                'cut -d, -f2'                          $'nodelim\n'
run 'C7 cut --output-delimiter (FLAG)' $'alice,30,nyc\nbob,25,la\n' "cut -d, -f1,3 --output-delimiter='|'" $'alice|nyc\nbob|la\n'

# --- tr ---
run 'T1 tr lower upper'       $'Hello World\n'                     "tr '[:lower:]' '[:upper:]'"           $'HELLO WORLD\n'
run 'T2 tr -d lower'          $'Hello World\n'                     "tr -d '[:lower:]'"                    $'H W\n'
run 'T3 tr -s space'          $'a   b    c\n'                      "tr -s ' '"                            $'a b c\n'
run 'T4 tr space underscore'  $'a b c\n'                           "tr ' ' '_'"                           $'a_b_c\n'
run 'T5 tr -cd digit (∅↵)'    $'abc123def456\n'                    "tr -cd '[:digit:]'"                   '123456'
run 'T6 tr digit # (repeat)'  $'id=42\n'                           "tr '[:digit:]' '#'"                   $'id=##\n'
run 'T7 tr -cd alnum (∅↵)'    $'a-b_c!d\n'                         "tr -cd '[:alnum:]'"                   'abcd'

# --- sort ---
run 'A1 sort'                 $'10\n2\n1\n20\n'                    'sort'                                 $'1\n10\n2\n20\n'
run 'A2 sort -n'              $'10\n2\n1\n20\n'                    'sort -n'                              $'1\n2\n10\n20\n'
run 'A3 sort -rn'             $'10\n2\n1\n20\n'                    'sort -rn'                             $'20\n10\n2\n1\n'
run 'A4 sort -t, -k2,2n'      $'charlie,3\nalice,1\nbob,2\n'       'sort -t, -k2,2n'                      $'alice,1\nbob,2\ncharlie,3\n'
run 'A5 sort -t, -k1,1'       $'charlie,3\nalice,1\nbob,2\n'       'sort -t, -k1,1'                       $'alice,1\nbob,2\ncharlie,3\n'
run 'A6 sort -u'              $'apple\napple\nbanana\n'            'sort -u'                              $'apple\nbanana\n'
run 'A7 sort -V'              $'v1.10\nv1.2\nv1.9\n'               'sort -V'                              $'v1.2\nv1.9\nv1.10\n'

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s) — see above"
exit "$fails"
