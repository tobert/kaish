#!/usr/bin/env bash
# Phase-2 runner: compare/patch (cmp/diff/patch). DEFERRED until the tree builds
# clean. File-oriented: materializes tempfile fixtures (honors $TMPDIR; not
# hardcoded /tmp). Classifies kaish against expectations-compare.md. No GNU.
# Temporary; delete with the sweep.
set -u
cd "$(dirname "$0")/../.." || exit 2
H=docs/builtin-sweep/harness.sh
KAISH="${KAISH:-./target/debug/kaish}"
fails=0
run() { "$H" "$@" || fails=$((fails+1)); }

FX=$(mktemp -d)
trap 'rm -rf "$FX"' EXIT
mkfixtures() {
  printf 'apple\nbanana\ncherry\n' > "$FX/A"
  printf 'apple\nBANANA\ncherry\n' > "$FX/B"
  printf 'apple\nbanana\ncherry\n' > "$FX/C"
  printf -- '--- a\n+++ b\n@@ -1,3 +1,3 @@\n apple\n-banana\n+BANANA\n cherry\n' > "$FX/P"
}
mkfixtures

# cmp — exit-code contract is the assertion that matters
run 'CM1 cmp identical (rc 0)' '' "cmp $FX/A $FX/C" '' 0
run 'CM3 cmp -s differ (silent, rc 1)' '' "cmp -s $FX/A $FX/B" '' 1
# CM2 cmp A B: message wording contested (byte vs char). OBSERVE + pin:
#   run 'CM2' '' "cmp $FX/A $FX/B" $'$FX/A $FX/B differ: byte 7, line 2\n' 1

# diff — identical + quiet are clean; default-format & unified header are observe-then-pin
run 'DF1 diff identical (rc 0)' '' "diff $FX/A $FX/C" '' 0
# DF2 default format: kaish emits UNIFIED, model reflex expects ed-style normal.
#   A mismatch here is the KNOWN unified-default divergence (DOC), not corruption.
# DF3 unified: assert the hunk; header (--- / +++) form (filenames? mtime?) is
#   kaish-specific — observe then pin:
#   run 'DF3 diff -u' '' "diff -u $FX/A $FX/B" $'<kaish unified header>\n@@ -1,3 +1,3 @@\n apple\n-banana\n+BANANA\n cherry\n' 1
# DF4 quiet (wording likely "Files X and Y differ"):
#   run 'DF4 diff -q' '' "diff -q $FX/A $FX/B" $'Files $FX/A and $FX/B differ\n' 1

# patch — apply, then read the file back to confirm the mutation
mkfixtures
run 'PT1 patch stdout' "$(cat "$FX/P")" "patch $FX/A" $'patching file '"$FX/A"$'\n' 0
run 'PT1b patch result content' '' "cat $FX/A" $'apple\nBANANA\ncherry\n' 0
# PT2 --dry-run: file must stay unmodified; message wording contested.
mkfixtures
run 'PT2b dry-run leaves file unmodified' '' "patch --dry-run $FX/A; cat $FX/A" $'apple\nbanana\ncherry\n' 0  # NB: combines; refine if dry-run prints to stdout

echo "---"
[ "$fails" -eq 0 ] && echo "no SILENT-WRONG cells" || echo "$fails SILENT-WRONG cell(s)"
exit "$fails"
