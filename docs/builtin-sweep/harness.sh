#!/usr/bin/env bash
# Model-consensus harness for the builtin sweep (temporary; delete with
# builtin-sweep.md before the next release).
#
# The reference is NOT GNU coreutils. We deliberately stay off GNU binaries so
# the sweep measures kaish against what the model panel (Sonnet + DeepSeek +
# Gemini) independently EXPECTS, not against an imported GNU implementation.
# The expected stdout is supplied by the panel's consensus; this script just
# runs kaish and classifies the divergence. The hunt target is SILENT-WRONG:
# kaish exits 0 but produces output the panel did not expect — the data-
# integrity hazard kaibo is exposed to. LOUD (kaish refuses) is an acceptable
# 80%-rule boundary; we still note it so P4 can check the message quality.
#
#   ./harness.sh '<label>' '<stdin>' '<kaish -c program>' '<expected stdout>' [expected-rc]
#
# expected-rc defaults to 0. Pass a nonzero value for cells the panel agrees
# SHOULD fail loud (a declared boundary) — then a kaish refusal is a MATCH.
#
# Verdicts:
#   MATCH         output and exit code match the panel expectation
#   SILENT-WRONG  kaish exit 0 but output != expectation        (P1 hazard)
#   LOUD          kaish errored where the panel expected output  (boundary/gap)
#   WRONG-RC      output matches but exit code disagrees
set -u
KAISH="${KAISH:-./target/debug/kaish}"
label=$1 stdin=$2 kprog=$3 expected=$4 exp_rc=${5:-0}

kerr=$(mktemp)
# Sentinel guard: bash $() strips trailing newlines, which would hide a
# trailing-newline divergence (e.g. tr -cd dropping the final \n) as a false
# MATCH. Append a sentinel, then strip exactly it, to capture bytes verbatim.
kout=$(printf '%s' "$stdin" | "$KAISH" -c "$kprog" 2>"$kerr"; printf X); krc=$?
kout=${kout%X}

if [ "$kout" = "$expected" ] && [ "$krc" -eq "$exp_rc" ]; then
  verdict=MATCH
elif [ "$kout" = "$expected" ]; then
  verdict=WRONG-RC
elif [ "$krc" -ne 0 ]; then
  verdict=LOUD
else
  verdict=SILENT-WRONG
fi

printf '%-14s %s\n' "$verdict" "$label"
if [ "$verdict" != MATCH ]; then
  printf '  kaish   (rc=%s): %q\n' "$krc" "$kout"
  [ -s "$kerr" ] && printf '  kaish err     : %s\n' "$(cat "$kerr")"
  printf '  expected(rc=%s): %q\n' "$exp_rc" "$expected"
fi
rm -f "$kerr"
[ "$verdict" = SILENT-WRONG ] && exit 1 || exit 0
