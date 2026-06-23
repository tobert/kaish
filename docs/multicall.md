# Multicall Binary + `execute_argv` — Design Doc

Status: **Design only — nothing landed (2026-06-23).** Two ideas, one cheap, one
load-bearing. (1) A busybox-style `kaish-multicall` binary that dispatches by
`argv[0]`: `ln -s kaish-multicall ~/bin/ln` makes `ln` run kaish's `ln` builtin
and exit. ~80 lines, no kernel changes — a third *frontend* alongside the REPL
and embedded clients. (2) The interesting half it exposes: kaish's embeddable
surface is **string-native** (`execute(&str)` → lex → parse → interpret). A
multicall binary — and any structured embedder — arrives with argv *already
tokenized by the OS*, and today has to re-quote it back into a string just so the
lexer can split it apart again. That round-trip is the exact footgun kaish exists
to avoid, and it's lossy for typed values. The fix is a peer entry point
`execute_argv`, not a rewrite. Driver: design check on the embeddable modes, plus
a real (if minor) capability gap for kaijutsu. `proptest` greenlit for the binder
equivalence tests (first use in the workspace — it's declared but unused).

## Motivation

A multicall binary is a third frontend. The embeddable seam already gives it
everything: `Kernel::new(KernelConfig::repl()/…)` to construct,
`kernel.execute("ln -s a b")` to run one line and get an `ExecResult`. The file
builtins (`ln`, `cp`, `mv`, `rm`, `mkdir`, `touch`) are **unconditionally
registered** (no feature gate), need only `localfs`, no `subprocess` — so a
multicall build can be `--no-default-features --features localfs` and still do
real-FS file ops. That the axes cut this cleanly is itself a good sign.

So the binary is trivial. What's worth writing down is the seam it pokes.

## The seam: string-native vs argv-native

### Why "argv is the primitive, string is the split-wrapper" is wrong

The tempting inversion — make `execute()` take an argv list and have
`execute_string()` be a thin wrapper that splits — has a category error in it. **A
kaish command string is not a list of arguments that needs splitting; it is a
program.** The proof is the `Stmt` enum (`ast/types.rs:12-51`):

```rust
enum Stmt { Command, Pipeline, If, For, While, Case,
            AndChain, OrChain, EnvScoped, ToolDef, ... }
```

`ln -s a b` parses to `Stmt::Command`, but `a | b && for x in $(c); do d; done` is
a tree of pipelines, chains, loops, and substitutions. You cannot "split" that
into a `Vec<String>` and reconstruct it — argv has no place to put a `for` loop or
a `|`. `execute(&str)` is therefore **strictly more expressive** than any argv
form; it cannot be *derived* from `execute_argv(split(s))`. Make argv the
primitive and the wrapper has to contain a whole parser — at which point it isn't
a wrapper.

### The two doors converge at the bottom, not the top

Both paths converge on one struct — `ToolArgs` (`kaish-types/tool.rs:317`) —
and one call, `tool.execute(tool_args, ctx)`:

```
execute(&str)          lex → parse → Vec<Stmt> → interpret
                          (pipelines, control flow, $(), globs, $VAR)
                                            │  per simple command
                                            ▼
                                   build_args_async(&[Arg]) ─┐
                                                             ├─► ToolArgs ─► tool.execute()
execute_argv(&[Value])  ── build_args_from_argv(&[Value]) ──┘     ▲
                          (no lex, no parse, no expansion)         │
                                                    validation · --json · latch · dispatch
```

The shared primitive is **command dispatch + arg binding**, sitting *under* both.
The doors are peers that merge late at `ToolArgs`. Neither wraps the other.

### What argv-native means semantically

`execute_argv` treats its tokens as **already-literal** — exactly like
single-quoted words: no glob expansion, no `$VAR` interpolation, no command
substitution, no word splitting. That isn't a limitation grafted on; it's *"no
word splitting, `$VAR` is one value"* taken to its end — the caller hands over the
exact tokens and kaish touches none of them. For multicall (`ln` receiving OS
argv) that's exactly right: the OS already resolved any glob, there's no shell to
interpolate against.

### The real payoff: typed passthrough

Take `&[Value]`, not `&[String]`. Today an embedder with a typed argument
round-trips through `to_argv()`, which **stringifies and loses** `Value::Bytes` /
`Value::Json` (the lossy-`to_argv` hazard already noted in CLAUDE.md).
`execute_argv(name, &[Value])` lets kaijutsu invoke one tool with a bytes blob or
a JSON record **without stringifying** — it lands straight in
`ToolArgs.positional: Vec<Value>`. A genuine capability gap closed, not just a
quoting bug dodged.

## Proposed API

```rust
pub async fn execute_argv(&self, name: &str, argv: &[Value]) -> Result<ExecResult>
```

The only new code is `build_args_from_argv` — it classifies argv into `ToolArgs`
using the same schema-driven flag/value/repeatable/`consumes` consumption that
`build_args_async` (`kernel.rs:3062`) already does for AST `Arg`s, minus the
`Expr` evaluation step (tokens are already values). From `ToolArgs` onward it
reuses **everything**: validation, `--json` (`GlobalFlags::apply_from_args`),
latch/nonce, `owns_output`, the snapshot/dispatch block
(`execute_command_depth`). No changes to the 70+ builtins. Effort: small — one
helper plus a thin public method.

### The one honest wrinkle

kaish has a *two-layer* arg model: the kernel binds `Arg → ToolArgs`, then each
clap builtin re-parses `to_argv(ToolArgs)` internally. So `build_args_from_argv`
must produce a `ToolArgs` whose `to_argv()` round-trips cleanly back through the
builtin's clap parser. For string positionals that's free; for typed `Value`s
it's exactly the `to_argv` stringification seam we're trying to avoid — so the
typed-passthrough win only fully lands for tools that read `args.positional`
directly (the documented clap pattern) rather than their clap struct. This is a
correctness *boundary* to pin with tests, not paper over.

## Test strategy

`execute_argv` joins the existing dispatch chain late, so "exhaustive" does **not**
mean re-running 3,500 tests through a new door. It means testing the one new seam
and proving **equivalence** to the door we already trust. Four surfaces, each with
a different honest ceiling:

### ① The new classifier — `build_args_from_argv` (proptest)

Near-pure function: `&[Value] + schema → ToolArgs`. Small and total, so it's the
one piece that can be *truly* exhaustively tested — and the oracle already exists
(`build_args_async` over the parsed string form). The property:

> for any argv with no shell metacharacters,
> `build_args_from_argv(argv, schema)` ≡
> `build_args_async(parse("<quoted argv>").args, schema)`

This is where `proptest` finally earns its keep (greenlit — first use in the
workspace; it's declared in the root `Cargo.toml` but unused). Generate random
`(builtin, flags, positionals)` against each real `ToolSchema` and assert the two
binders converge. Hand-written `#[case]`s can't cover the flag/value/repeatable
consumption matrix (`-A N`, `-e … -e …`, glued `-f1`); a generator can.

### ② Convergence — argv door ≡ string door, end to end

The high-value differential, and it rides our biggest existing asset for free:
every single-command kernel-routed test is already an oracle. A harness that, for
a corpus of `(string, expected ExecResult)` cases where the string is one
`Stmt::Command`, derives the argv and asserts the argv-form yields the identical
`ExecResult`. The ~1,344 integration cases' simple-command subset becomes the argv
suite at near-zero authoring cost — as close to exhaustive-over-the-builtin-set as
this codebase gets.

The boundary is clean and itself testable: pipelines, `&&`/`||`, control flow,
`$()`, and **globs/`$VAR`** are *not expressible* through `execute_argv` (there's
no string to parse, so they can't enter). Pin that as a property — argv tokens are
literal: `execute_argv("echo", ["*.txt"])` emits `*.txt`, does not glob. That's
the semantic contract, not an accident.

### ③ Typed passthrough — `Bytes`/`Json` survive without `to_argv()` loss

**Cannot** be exhaustive (the `Value` space is unbounded) and shouldn't pretend.
The precise risk is the `to_argv()` stringification seam (see the wrinkle above).
Pin the cases: a `Bytes` positional round-trips intact through a builtin that
reads `args.positional`; *assert the known failure* (or an explicit reject) for
one that reads its clap field. Boundary tests, not a coverage sweep.

### ④ The shared tail — validation, `--json`, latch, dispatch

**Deliberately not** re-tested exhaustively. `execute_argv` reuses it from
`ToolArgs` onward, so a handful of smoke tests prove *reachability* (a latched `rm`
via argv still emits a nonce; an argv `ls` with `--json` still applies output
format) and the convergence property ② covers the rest transitively. Re-running
the whole `--json` sweep through the new door would be cargo-culting.

### Concrete test plan

- `tests/execute_argv_tests.rs` — kernel-routed happy/error/latch/`--json` smoke
  (existing `common::kernel_at`/`run` harness, ~20 cases).
- `tests/execute_argv_equivalence.rs` — the differential ② over the single-command
  corpus.
- a `proptest` module for ① (binder equivalence against `build_args_async`).
- `Bytes`/`Json` round-trip pins for ③.

bash differential (`shell_compat!`) does **not** apply here — bash has no
`execute_argv` analogue.

## Open questions / deferred

- **`&[String]` vs `&[Value]` signature.** `&[Value]` is the right primitive (it's
  the only thing that buys the typed-passthrough win); a `&[String]` form, if
  wanted for multicall ergonomics, is a trivial `.map(Value::String)` wrapper over
  it. Decide whether to expose both.
- **Does the multicall binary live in its own crate or as a second `[[bin]]` in
  `kaish-repl`?** Lean: second bin first, promote to a crate only if it grows.
- **Quoting-reconstruct interim.** If the multicall binary ships *before*
  `execute_argv`, it must reconstruct a quoted string — correct but ugly. Prefer
  to land `execute_argv` first so the binary is argv-native from day one.

Author: design session 2026-06-22/23 (multicall curiosity → embeddable-modes
design check).
Related: [EMBEDDING.md](EMBEDDING.md), [LANGUAGE.md](LANGUAGE.md),
[binary-data.md](binary-data.md) (the `to_argv` lossiness / typed-`Value`
context), [issues.md](issues.md). CLAUDE.md "Contributor conventions" for the
two-layer clap arg model and `to_argv()` gotchas.
