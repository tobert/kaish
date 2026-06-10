# Cross-Model Evaluation: kaish Collection Syntax

*Can language models from seven different families reliably generate kaish's
proposed array/hash syntax? We tested. This is the scorecard, the journey behind
it, and how to reproduce it.*

Companion to [`designing-syntax-with-llms.md`](designing-syntax-with-llms.md) (the
methodology) and [`arrays-and-hashes.md`](arrays-and-hashes.md) (the design the
evals shaped).

---

## TL;DR

On the **final** collection syntax, with a **complete** cheat-sheet, every model from
flagship cloud down to a 4-bit-quantized 26B local one scored **12/12**. The first to
crack was a ~4B model — and it cracked on basic shell `$`-sigils, *not* on the new
syntax. The interesting result isn't the clean sweep; it's *what it took to get there*
and *where the floor is*. Earlier rounds, with weaker syntaxes and incomplete specs,
failed — and the failures clustered almost entirely in the small/fast models. The
lesson:

> Capability tier predicts a model's robustness to a *bad* spec, not to a *good*
> one. A complete spec flattens the tiers. So design and document for the weak tail,
> and the flagships come along for free.

---

## The panel

| Model | Vendor | Tier | Mode | Access |
|---|---|---|---|---|
| DeepSeek-V4-Pro | DeepSeek | flagship | thinking on | API (dpal) |
| DeepSeek-V4-Flash | DeepSeek | fast | thinking **off** | API (dpal) |
| Gemini 3 Pro | Google | flagship | thinking high | API (gpal) |
| Gemini Flash-Lite | Google | fast | default | API (gpal) |
| Claude Haiku 4.5 | Anthropic | fast | no extended thinking | subagent |
| Gemma-4-26B-A4B (Q4) | Google | local/quantized | thinking on | local Lemonade |
| Gemma-4-E4B (Q4) | Google | local/tiny (~4B eff.) | direct | local Lemonade |
| GLM-4.5-Air (Q4) | Zhipu | local/quantized | — | **failed to load (VRAM)** |
| Qwen3-Coder-Next (Q4) | Alibaba | local/quantized | — | **failed to load (VRAM)** |

The local models run on [AMD Lemonade](https://github.com/lemonade-sdk/lemonade)
(llama.cpp backend, OpenAI-compatible) in a container. Cross-vendor breadth and a
deliberate spread of capability tiers are the whole point — agreement *across*
families is a far stronger signal than one model nodding, and the cheap/fast models
are where designs actually break. (GLM-4.5-Air and Qwen3-Coder-Next wouldn't fit in
VRAM alongside a 256K context — a reminder that "local" has a hardware ceiling.)

---

## The task set

One prompt, twelve tasks, each probing a distinct piece of the syntax. "Output code
only, no prose." Identical across every model.

| # | Probes | Canonical answer |
|---|---|---|
| 1 | list index, 0-indexed | `echo ${colors[1]}` |
| 2 | negative index | `echo ${colors[-1]}` |
| 3 | record iteration + dynamic key in a string | `for k in keys $inv; do echo "${k}: ${inv.$k}"; done` |
| 4 | record length | `echo ${#inv}` |
| 5 | slice | `echo ${nums[0:2]}` |
| 6 | record field set + read | `inv.bananas=7; echo ${inv.bananas}` |
| 7 | in-place `push` in a loop + scalar `!=` | `for c in $colors; do if [[ $c != green ]]; then push picks $c; fi; done; echo ${#picks}` |
| 8 | spread / flatten | `flat=[...$a ...$b]; echo ${#flat}` |
| 9 | nest (bare var = one element) | `nested=[$a $b]; echo ${#nested}` |
| 10 | chained dot + index | `echo ${servers.web[0]}` |
| 11 | membership operator | `if [[ green in $colors ]]; then echo "has green"; fi` |
| 12 | nested record access | `echo ${users.alice.city}` |

Key design properties under test: braced `${…}` for *all* path/index access (bare
`$x` = whole value); `${#x}` for length; `keys`/`values` as builtins; `[[ in ]]`
membership; in-place `push` taking a bareword name; nest-by-default with `...`
spread; no-space assignment; optional commas.

---

## Results — the final syntax

| Task | DS-Pro | DS-Flash | Gem-Pro | Gem-Lite | Haiku | Gemma-4† |
|---|:--:|:--:|:--:|:--:|:--:|:--:|
| 1 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 2 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 3 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 4 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 5 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 6 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 7 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 8 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 9 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 10 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 11 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 12 | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Σ** | **12** | **12** | **12** | **12** | **12** | **12** |

*† Gemma-4-26B graded from its (correct) reasoning trace — see "Operational
gotchas". GLM-4.5-Air and Qwen3-Coder-Next couldn't be loaded (VRAM).*

Six models, every tier, every vendor: **72/72**. Notably uniform — all six even
converged on the same idioms unprompted: `[$a $b]` for nesting vs `[...$a ...$b]`
for flattening (tasks 8/9), bare `$c` for the scalar loop var but braced
`${inv.$k}` for the dynamic-key path-in-a-string (task 3), and `[[ $c != green ]]`
for scalar inequality (task 7).

That last one matters, because task 7 is the *only* place these six models ever
disagreed. But there's a floor below them, and it's worth seeing.

### The capability floor: Gemma-4-E4B (~4B)

The tiny Gemma sibling scored **9/12** — and *where* it failed is the finding. It got
every braced `${…}` form right (`${colors[1]}`, `${#inventory}`, `${nums[0:2]}`,
`${servers.web[0]}`, `${users.alice.city}`, `[...$a ...$b]` vs `[$a $b]`), and missed
exactly the three tasks that lean on a bare `$`-sigil reference:

```sh
# task 3:  for k in keys inventory      (should be `keys $inventory`; also ${inventory.k} dropped the $)
# task 7:  for c in colors; [[ c != green ]]; push picks c   (every $ missing)
# task 11: [[ green in colors ]]         (should be `$colors`)
```

It systematically dropped the `$` on **bare variable references** — `keys $inv`,
`$colors`, `$c` — while nailing the novel collection syntax. So at ~4B the failure mode
isn't our design at all; it's **vanilla shell `$`-sigil discipline**. The collection
syntax sits *above* the floor. That's a reassuring place for it to be: the thing that
breaks first is the thing every shell already requires, not anything we added.

---

## The journey (where the models actually diverged)

A wall of checkmarks is a boring artifact of a *finished* design. The signal lived
in the rounds before this one, and it was concentrated in the fast tier.

**`has` command → `[[ in ]]` operator.** An early membership design used a `has`
command. Flagships handled it; the fast models did not, and *differently*:

```sh
# DeepSeek-Flash wrapped it as a value:
if $(has $colors green) { ... }
# Gemini-Lite garbled the arguments:
if has keys $inventory "bananas" { ... }
```

Re-spelled as `[[ green in $colors ]]`, both fast models went 7/7. (Task 11 above
is the descendant — now unanimous.)

**Nested capture broke the cheapest model.** A `len $(keys $r)` construct — counting
a record's keys via a nested builtin call — was collapsed by Gemini-Lite into
`echo len keys $inventory`. We removed the nesting by making length the
param-expansion `${#r}` (task 4), and the failure vanished.

**The one-line fix that erased task 7.** Earlier specs showed `[[ k not in $r ]]`
for records but no scalar `!=`. Asked to "push each color that is NOT green," models
agonized. Gemma-4's reasoning trace is the clearest exhibit — it visibly
deliberated:

```
*   if [[ $c != green ]]; then ... ? Or [[ $c not in [green] ]]?
*   The cheat sheet says [[ k not in $r ]]. It doesn't show != ...
*   Let's use [[ $c != green ]].
```

It landed right, but only after thrashing; smaller models sometimes didn't. Adding a
single line to the cheat-sheet — `For inequality of two scalars use !=: [[ $x != green ]]`
— made all six models nail task 7 on the first try. One line of spec erased the
entire divergence. (This is also why example/spec **completeness** beats model
strength: the gap, not the model, was the problem.)

**The flagship is robust to almost anything.** As a control, we fed Haiku the
*discarded* syntaxes — the `has` command, bare access, nested `len`, even 1-indexed
lists and implicit splat. It scored 8/8 on all of them. Its single stumble was,
again, a spec gap: with no empty-list example shown, it leaked bash array syntax
(`keys_list=()`). The capable models were always going to be fine. The design choices
were never *for* them.

---

## Operational gotchas (the local-model tax)

Worth recording for anyone running evals against quantized local models via Lemonade
/ llama.cpp:

- **Reasoning models hide the answer.** Gemma-4 and GLM-4.5-Air are served with a
  thinking channel: the chain-of-thought lands in `reasoning_content` and only the
  *final* answer in `content`. Gemma-4 is verbose enough that with a small output
  budget it hit the token cap mid-thought and `content` came back **empty** —
  21,000+ characters of correct reasoning, zero in the field you'd normally grade.
  Fix: a large `max_tokens` (and a large server context — our first container was
  capped too low), or grade the reasoning trace directly.
- **Forcing no-think trades accuracy for brevity.** Setting
  `chat_template_kwargs={"enable_thinking": false}` made Gemma-4 answer directly —
  and *sloppier*: it dropped a sigil (`${inv.k}` instead of `${inv.$k}`) and mangled
  task 7 to `[[ c not in green ]]` (missing `$`, wrong operator). With thinking on,
  both were correct. For a reasoning model, the deliberation is doing real work;
  amputating it costs accuracy.
- **Big context doesn't fix a runaway CoT.** After we bumped the server to a 256K
  context, Gemma-4-26B *still* overflowed its 22k-token output budget thinking-on —
  the context cap was never the real limit; the model just deliberates past any sane
  `max_tokens` on a 12-task batch. Truncation and verbosity are different problems.
- **"Local" has a hardware ceiling.** GLM-4.5-Air (~68 GB) and Qwen3-Coder-Next both
  returned `model_load_error: llama-server failed to start` — they don't fit in VRAM
  alongside a 256K KV cache. Half the local panel was settled by `nvidia-smi`, not by
  syntax. Plan eval coverage around what actually loads.
- **Discovery, not assumption.** Lemonade wasn't on the documented default port —
  it was on `:13305`. A quick `/v1/models` sweep across listening ports found it
  (and surfaced the bonus models sitting alongside Gemma). Probe; don't assume.

---

## Reproduce it

The harness is deliberately low-tech: stateless one-shot calls, one self-contained
prompt, grade the output. Three transports, one prompt:

- **Cloud models** — OpenAI-/vendor-compatible one-shot calls (here via `dpal`/`gpal`
  MCP tools; any client works).
- **Local models** — `curl`/`urllib` POST to the Lemonade OpenAI-compatible endpoint
  (`http://localhost:13305/api/v1/chat/completions`), model id from `/v1/models`.
  Pull code blocks from `content` *or* `reasoning_content` so reasoning models stay
  gradeable.
- **Claude** — a Haiku subagent with the same prompt.

The canonical prompt is the 12-task cheat-sheet above; the exact text lives with the
eval scripts. Grading is per-task pass/fail by hand (12 cells/model) — small enough
to eyeball, and eyeballing is where you *notice* the divergences that matter.

Total cost for this round: a few dozen one-shot calls and a couple of local
inferences — minutes, not hours.

---

## Takeaways

1. **The final kaish collection syntax is model-portable.** 12/12 across six models
   spanning three vendors and flagship → fast → mid-size-local tiers. No vendor's
   training prior fights it — and the first model to miss anything (a ~4B local) missed
   on shell `$`-sigils, not on the syntax. The design sits *above* the capability
   floor.
2. **Divergence is a spec gap wearing a model costume.** Every failure we saw traced
   to an under-specified corner, and fixing the spec fixed every model at once.
3. **Tier predicts robustness-to-bad-spec, not robustness-to-good-spec.** Design for
   the weak tail; the flagships are along for the ride.
4. **Completeness > strength.** A single missing `!=` line caused more failures than
   any syntax choice. The capable model still face-plants on a hole in the docs.
5. **Mind the reasoning channel** when grading local/quantized models, or you'll
   score a correct model as a blank.

---

*Panel: DeepSeek V4 (Pro/Flash), Gemini 3 (Pro/Flash-Lite), Claude Haiku 4.5,
Gemma-4-26B-A4B (Q4, local) — all 12/12; Gemma-4-E4B (Q4, local) 9/12 (sigil floor);
GLM-4.5-Air and Qwen3-Coder-Next (Q4, local) failed to load. June 2026. Method:
[`designing-syntax-with-llms.md`](designing-syntax-with-llms.md).*
