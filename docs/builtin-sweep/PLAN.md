# Builtin Sweep — Execution Plan (ephemeral)

**Status: DRAFT — reviewed (DeepSeek via kaibo, 2026-06-16); see §Review findings.
rg removal CONFIRMED (Amy override). The tee/patch latch graft still needs design
work before execution — it's a new mutation-gate path, not a reuse of `rm`'s.**

This is the playbook for turning the banked builtin-sweep catalog into a landed
**full burn-down**. It is written for a *fresh executing session* that does not
share the planning conversation — it has only this repo (CLAUDE.md + auto-memory)
and the `docs/builtin-sweep/` artifacts. Temporary; **delete with the rest of
`docs/builtin-sweep/` before the next release** (Phase E).

## Roles & scope
- **Planning session** (authored this): owns the plan until human sign-off. Does
  NOT run Phase 2 or write fixes.
- **Executing session(s)**: run this plan **continuously** — do not stop between
  phases. Resume across passes (context limits will force a few) by reading the
  Status table (§Status), which you update at the end of each pass. **Pause only
  for a tough UX/design decision** (escalate to the human) or the final sign-off.
- **PR**: #7, branch `docs/builtin-sweep`, base `main`, currently **draft**.
  Worktree: `/home/atobey/src/kaish-builtin-sweep`.
- **Scope: FULL burn-down** — Phase-2 verdicts + every CODE fix + the DOC updates
  that ride with them, checked off, scaffolding deleted, PR marked ready &
  reviewed. The awk PR (#6) is the model.
- **Out of scope**: FS mutators (cp/mv/rm/ln/mkdir/touch/write) — separate
  behavioral pass. `format_string` (not a builtin).

## Decisions (pre-resolved by the human — implement, don't re-litigate)
1. **Drop `rg`.** It duplicates `grep`'s engine with a divergent, hesitation-
   inducing surface. Remove the builtin (delete `rg.rs`, deregister it, drop it
   from help/`LANGUAGE.md`/README builtin lists, add a `CHANGELOG.md` **Removed**
   bullet — this is breaking). The search cluster's `R*` cells become a
   removal-safety check, not a fix target; the no-match-rc and ERE-dialect findings
   carry over to `grep`.
2. **`diff` default = unified.** Keep the unified default (it's what `git diff`
   shows and what's useful for `patch`). Just confirm + document it; **do not chase
   richer diff features now** — deeper `diff` work is a deliberate later pass, so
   don't rat-hole. The model-reflex "ed-style normal default" is a documented
   divergence, not a bug.
3. **`tee` / `patch` follow the same rules as `rm`.** Their file mutations must go
   through the destructive-op machinery: honor `set -o latch` (confirmation nonce)
   and trash-on-overwrite, the same path `rm` uses (`decide_rm_action` /
   `NonceStore` / trash). Today they route through the VFS (overlay-safe ✅) but
   **bypass the latch/trash** — close that. This is a real CODE item, likely P1/P2,
   and the same hazard class as sed's deferred `-i` (`[[sed_ergonomics_pass]]`).

## Inputs (already on the branch)
- `README.md` — method + cluster tracker + accumulated cross-tool levers.
- `battery-<cluster>.md` — fixed cells + the verbatim panel prompt (7 clusters).
- `expectations-<cluster>.md` — banked 3-vendor consensus, each finding routed
  **CODE** (fix kaish) / **DOC** (decide + document). Holds per-cell expected
  output, contested cells, and per-cluster "Phase-2 watch lists".
- `run-<cluster>.sh` — deferred Phase-2 runners (assert high-confidence cells;
  contested/format cells commented with observe-then-pin notes).
- `harness.sh` — the no-GNU classifier: runs `kaish -c <prog>` on supplied stdin,
  compares to a **panel-supplied expected string** (NOT a GNU run) →
  `MATCH | SILENT-WRONG | LOUD | WRONG-RC`. Newline-exact (sentinel guard).

## Ground rules (non-negotiable)
1. **Model-consensus is the oracle, never GNU** (`[[model-memory-over-gnu-oracle]]`).
   Don't add a GNU binary to any test/runner. If a banked expectation looks wrong,
   re-derive via the panel (`kaibo oneshot`, casts deepseek/gemini + a Sonnet
   subagent), don't "check it against gawk".
2. **Hunt target = SILENT-WRONG.** kaish exit 0 + wrong bytes is the data hazard.
   LOUD (kaish refuses) is an acceptable 80%-boundary, but its *message* must hint
   the fix. A silent divergence is itself a P1.
3. **TDD, kernel-routed.** Every CODE fix lands with a test asserting
   `kaish == banked consensus`, routed through `kernel.execute(...)`, NOT a
   builtin's `.execute()` (CLAUDE.md). Failing test first; it must be able to fail.
4. **Docs ride with the fix.** kaish's consumers read its help system, so a help
   update ships the correction to them for free. Every correction updates the
   relevant `crates/kaish-help/content/en/*.md` fragment (+ `fragments.rs`/regen if
   it's syntax) and adds a `CHANGELOG.md` `[Unreleased]` bullet **in the same
   commit**. No separate "docs phase".
5. **Small, contained commits.** One logical fix (+ its test + its doc) per commit.
   Conventional message, Co-Authored-By trailer. Don't force-push once Phase D
   starts landing.
6. **Gates before every commit:** `cargo test --all` and
   `cargo clippy --all --all-targets` both clean (0 warnings); the
   `--no-default-features` gates too if you touch capability-gated code.
7. **CLAUDE.md gotchas:** clap-parsed-vs-raw flag reads; `--json` is kernel-level;
   `with_output` drops `rich_json` (use `with_output_and_text`); `to_argv()`
   injects `--`; no real system paths in tests (tempfile / `/v/...`).

## Execution
Run straight through A→E. The only mandatory human gate is **Phase E sign-off
before merge**. Otherwise pause **only** when you hit a tough UX/design decision
(§When to pause). Update §Status each pass so the next session resumes cleanly.

### Phase A — Pre-flight + harness validation
1. `cargo build -p kaish-repl` → `./target/debug/kaish`; confirm clean.
2. Run `run-pilot.sh` (cut/tr/sort) — the first real kaish run; validates the
   harness end-to-end.
3. Sanity-check: cells should be `MATCH` or an explainable divergence. If the
   *harness itself* misbehaves (quoting/sentinel/stdin), fix `harness.sh` and note
   it. Real `SILENT-WRONG` on a "unanimous" cell = a genuine finding; keep going.

### Phase B — Phase-2 run (all clusters → verdicts)
1. Run each `run-<cluster>.sh`; capture verdicts.
2. **Contested/format cells** (commented in runners): run kaish manually, observe
   the bytes, **pin per the 80/20 rule** (§When to pause) — see each
   `expectations-*.md` "Phase-2 watch list" (wc/`uniq -c` padding, `cat -n` width,
   `xxd` dump spacing, `diff -u` header, `checksum` line shape, `cmp` wording,
   base64 wrap).
3. **Guarded cells:** `cat /dev/zero` (CT5) under `timeout 5` — a timeout is a FAIL
   (the safety error regressed). LOUD cells (split/dd/jq/sha256sum) — assert rc≠0
   AND a hinted message.
4. **Writer behavioral checks:** `tee`/`patch` under `--overlay` (writes captured?)
   and `set -o latch` (currently NOT gated — that's Decision #3 to fix).

### Phase C — Triage → punch list
Create `docs/builtin-sweep-overhaul.md`, the **twin of `docs/awk-overhaul.md`**
(issues.md-style, P1–P4, delete-before-release). One section per priority; each
item: tool · symptom (failing cell) · verdict · contributing factor · fix sketch ·
the test to write. Seed it with the pre-resolved Decisions (rg removal, tee/patch
latch+trash) and the cross-tool levers from `README.md`. Priorities:
- **P1** — SILENT-WRONG + silent contract breaks: exit-code contracts
  (grep/cmp/diff 0/1/2), silent namesake misbehavior, jq `env` silent-null,
  tee/patch bypassing latch/trash, any silent corruption.
- **P2** — fidelity (output materially wrong).
- **P3** — coverage gaps (a reached-for form that errors and shouldn't).
- **P4** — DOC-only decisions + loud-boundary message quality + format pins that
  weren't auto-pinned in Phase B.

### Phase D — Fix loop (the bulk; several passes)
Work **P1 → P2 → P3 → P4**. Per item: failing kernel-routed test → implement →
green → doc + CHANGELOG in the same commit → check the box. Small contained
commits; gates green each time. Resume via §Status across passes.

### Phase E — Land
1. Confirm every box in `builtin-sweep-overhaul.md` is checked (or explicitly
   waived with a reason).
2. **Delete the scaffolding**: the entire `docs/builtin-sweep/` directory
   (batteries, expectations, runners, harness, README, this PLAN) AND
   `docs/builtin-sweep-overhaul.md`. Durable record = code + tests + CHANGELOG +
   help content.
3. Final gates green.
4. `gh pr ready 7`; request review (`/code-review` or another model).
5. **⛳ Human sign-off before merge.**

## When to pause and ask (vs decide yourself)
- **Pin yourself** (no pause): a format/behavior choice that clearly fits the 80/20
  rule, matches a banked consensus, or where you're confident — pin kaish's
  behavior (fix if broken, else document current) and move on.
- **Pause and ask**: a genuine UX/design tradeoff with no clear 80/20 answer, a
  user-visible breaking change beyond the pre-resolved Decisions, or anything where
  reasonable models would disagree on what kaish *should* do. Surface it crisply
  with options; don't guess.

## Status (executor updates every pass)
| Phase | State | Pass / notes |
|-------|-------|--------------|
| A pre-flight + harness | ☑ done | Harness stdin-delivery was broken: `kaish -c` ignored process stdin → all cells empty/rc0. **Amy chose "fix the REPL"** not workaround. Fix shipped `c09ee26` (ExecuteOptions::stdin + REPL read_piped_stdin, DeepSeek-reviewed). Harness reverted to natural piping. Pilot: 13 MATCH + 8 real findings. |
| B run all clusters | ☑ done | All 7 clusters run (rc-fixed harness `225e1c5`). Clean: grep/cmp/diff-id/printf/seq/sort-core/tr-core/tee. Findings catalogued in builtin-sweep-overhaul.md. |
| C punch list | ☑ done | `docs/builtin-sweep-overhaul.md` written. Two Amy decisions: comma→message+docs (keep `,` reserved for brace-exp + arrays/hashes); trailing-newline→EMIT. |
| D P1 | ☐ | |
| D P2 | ☐ | |
| D P3+P4 | ☐ | |
| E land | ☐ | |

## Harness gotchas (so the executor doesn't relearn them)
- `$()` strips trailing newlines → harness uses a sentinel; trust its `∅↵` cells.
- Binary output routes through `xxd -p`/base64 (done in run-binary.sh) — never
  assert raw bytes through `$()`.
- LOUD cells: pass `expected-rc` to assert the refusal; eyeball stderr for a hint
  (harness checks rc+stdout, not stderr text).
- `diff A B` default = unified in kaish but the panel expects ed-style normal —
  **DOC divergence, not corruption** (Decision #2); don't file it P1.
- File runners (`run-compare.sh`/`run-io.sh`) build tempfile fixtures (honor
  `$TMPDIR`, never hardcode `/tmp`); writer cells recreate fixtures + read back.

## Review findings (DeepSeek via kaibo, 2026-06-16)
Reviewed by a memoryless-session lens against the real builtin code. Verdicts:

1. **rg has features grep lacks** — `--type`/`--type-not`, `--hidden`,
   `--max-count`, `--files`, `-P`/`--pcre2`, `--no-ignore`. **OVERRIDDEN by Amy
   2026-06-16: remove rg regardless** (80% rule — grep is enough; the second tool's
   divergent surface earns its keep poorly). The lost features are an **accepted
   regression**: list them in the `CHANGELOG.md` **Removed** bullet so it's honest;
   do NOT port them to grep first. Useful removal touch-list from the review:
   `rg.rs`, the orphaned help page `crates/kaish-help/content/en/rg.md`, the
   `grep_engine.rs` "used by both grep and rg" comment + its `#![allow(dead_code)]`
   (remove the allow after deletion so newly-dead code surfaces), the `grep-pcre2`
   dep + `pcre2` feature in `crates/kaish-kernel/Cargo.toml`, and `Cargo.lock`.
2. **tee/patch latch+trash is a NEW gate path, not a reuse of `rm`'s** (VALID —
   real design work). `decide_rm_action` → `{Trash, Delete, Latch}`; none means
   "trash the existing content, *then* overwrite". For rm, trash IS the operation;
   for tee/patch it's a pre-write safety copy that has no code path today. Open
   design Qs to resolve in Phase C before coding: a `decide_mutation_action` →
   `{TrashFirst(path), Latch, Proceed}` (same priority chain + `/tmp`,`/v` excludes,
   different action types); tee can create a nonexistent file (no trash, just
   write) where rm requires existence; **overlay semantics** — should latch/trash
   even fire on ephemeral overlay writes that only materialize on `kaish-vfs
   commit`? Nonce `command`/`paths` scope; `--confirm` flag name (tee/patch have
   none today); does `tee -a` (append, additive) gate?
3. **diff = unified is already implemented** (`diff.rs`) — Decision #2 is
   **doc-only**, no code. Don't frame it as work.
4. **grep exit codes are already correct** (rc 1 no-match, rc 2 error). Phase C/D
   must distinguish "add a kernel-routed test PINNING existing-correct behavior"
   from "fix a bug" so the executor doesn't hunt a non-bug.
5. **Harness:** mandate **byte-exact (`cmp`)** comparison for binary cells (string
   equality can false-pass on NUL/encoding); stderr-hint quality can't be judged
   mechanically — keep it a P4 human/LLM eyeball.

## Definition of done
`builtin-sweep-overhaul.md` empty; `rg` removed; tee/patch latch+trash-gated; all
CODE fixes landed with passing kernel-routed tests; corrections documented in help
+ LANGUAGE.md + CHANGELOG (shipped alongside each fix); `docs/builtin-sweep/`
deleted; PR #7 green, ready, reviewed, merged.
