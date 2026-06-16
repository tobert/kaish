# Builtin Sweep — session signoff (2026-06-16)

Branch `docs/builtin-sweep`, PR #7 (draft). Picking up: read this, then
`docs/builtin-sweep/PLAN.md` (Status table) + `docs/builtin-sweep-overhaul.md`
(punch list). **Delete this signoff.md when you resume** — it's ephemeral.

## TL;DR
Phases A–C done. Phase D started: `cat` byte-concat corruption fixed + reviewed.
**Then I found a regression in my OWN earlier stdin fix — it's the #1 next task.**
Nothing is merged (draft PR), so nothing shipped broken.

## ⚠ TOP PRIORITY next: fix the stdin eager-read hang (punch list P1.0)
The REPL stdin fix I landed in `c09ee26` makes `kaish -c '<prog>'` eagerly
`read_to_end` process stdin *before* executing. That **hangs** `kaish -c 'echo hi'`
when stdin is an open non-TTY pipe that never sends EOF (subprocess-with-inherited-
idle-stdin — the way the Bash tool, CI, editors, etc. spawn it). Proof:
`sleep 10 | kaish -c 'echo hi'` → no output, rc 124 (hung); `sleep 10 | bash -c
'echo hi'` → prints `hi` immediately (bash never reads stdin for echo).

- **Blast radius:** standalone `kaish -c`/script CLI only. Embedders
  (kaijutsu/kaibo) feed stdin via the kernel API → unaffected. The sweep harness
  pipes closed stdin (EOF) → unaffected, so Phase D can continue regardless. But
  this MUST be fixed before PR #7 is marked ready.
- **Correct fix = lazy, pipe-backed stdin** (details in punch list P1.0): feed
  process stdin to the kernel as a lazy `PipeReader` seeding `exec_ctx.pipe_stdin`
  via a detached copy thread, so a command that doesn't read stdin (`echo`) never
  blocks. Obstacle: `ExecuteOptions::stdin` is `Option<String>` in kaish-types and
  can't hold a kernel `PipeReader` — needs a new seam (a `Kernel`/`EmbeddedClient`
  method taking a boxed `AsyncRead`, or a kaish-types reader trait). Keep the
  String path for embedders with a ready buffer.
- DeepSeek flagged eager read-to-EOF in its review of the original fix; I wrongly
  judged it acceptable. It isn't — it breaks the most common CLI invocation.
- **Decision for whoever resumes:** this redesign touches the embedder API surface
  again (new stdin seam). Worth a quick confirm with Amy on the seam shape before
  coding, then TDD + DeepSeek review like the rest.

## What landed this session (all committed, gates green, DeepSeek-reviewed where noted)
1. `c09ee26` feat(repl): read piped stdin in -c/script mode — ExecuteOptions::stdin
   + with_stdin, kernel StdinGuard + execute_pipeline consume-once seeding, REPL
   read_piped_stdin. **Has the P1.0 regression above.** DeepSeek-reviewed (the
   review caught the eager-read risk; I under-weighted it).
2. `225e1c5` test(builtin-sweep): harness rc-capture fix + tee runner quoting. The
   sentinel `printf X` clobbered `$?` so every rc verdict was bogus (loud refusals
   mis-filed as SILENT-WRONG). Fixed → real verdicts.
3. `6a5368e` docs(builtin-sweep): Phase C punch list + Amy decisions.
4. `995a744` fix(cat): concatenate multiple files byte-verbatim (P1.1). cat inserted
   a `\n` separator between files, corrupting the byte stream. DeepSeek-reviewed.

## Phase status
- **A pre-flight + harness — DONE.** The harness's original stdin delivery was
  broken (`kaish -c` ignored process stdin); Amy chose "fix the REPL" over a
  workaround → led to `c09ee26` (and its P1.0 regression).
- **B run all clusters — DONE.** 7 clusters run with the rc-fixed harness. Clean
  (confirm-and-pin, no bugs): grep (incl. no-match rc=1, `-E` alternation, ERE
  `a{2}`), cmp, diff-identical, printf, seq, sort-core, tr-core, tee.
- **C punch list — DONE.** `docs/builtin-sweep-overhaul.md`. Two Amy decisions:
  unquoted comma stays reserved (brace-exp + arrays/hashes) → fix is message+docs
  not lexer (P4.3); trailing newline → EMIT to match consensus (P4.1).
- **D fix loop — IN PROGRESS.** P1.1 (cat) done. Remaining below.

## Remaining Phase D work (priority order)
- **P1.0** stdin eager-read hang — the redesign above. **Do first.**
- **P1.2** tee/patch latch+trash gating (Decision #3) — design item with open Qs
  (decide_mutation_action shape, overlay-write semantics, --confirm naming, does
  `tee -a` gate). Likely needs an Amy design pass. See punch list.
- **P2** (clear fidelity fixes, TDD each): tail `-n +N` start-offset (read raw arg,
  the `+` is lost by clap `i64`); head `-n -N` all-but-last-N; cut prints
  no-delimiter line under `-f` w/o `-s`; jq `-c` compact ignored; split `--limit`
  off-by-one; wc leading-tab + trailing newline; tr `-c`/`--complement` missing.
- **P3** sort `-V` version sort.
- **P4** trailing-newline EMIT on tac/wc/split/jq/base64/xxd/dd (decided); patch
  extra "N changes applied" line; comma error-message + cut/sort help (P4.3); diff
  unified-default doc (#2); **rg removal (#1)** — delete rg.rs + help/Cargo/lock,
  CHANGELOG Removed bullet listing accepted-lost features.

## Method reminders (hold the line)
- Oracle = model-consensus (kaibo), NOT GNU. TDD, kernel-routed tests
  (`kernel.execute(...)`). Docs+CHANGELOG ride each fix's commit. Gates:
  `cargo test --all` + `cargo clippy --all --all-targets` clean. **DeepSeek review
  (kaibo cast=deepseek) before committing each chunk** (Amy's instruction).
- Re-run a cluster after a fix via `docs/builtin-sweep/run-<cluster>.sh` (rebuild
  `cargo build -p kaish-repl` first — the runners use `./target/debug/kaish`).
- Phase E deletes ALL of `docs/builtin-sweep/` + `docs/builtin-sweep-overhaul.md`
  + this signoff.md; durable record = code + tests + CHANGELOG + help.
