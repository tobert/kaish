# Plan: Here-string `<<<` and `jq --var`

Close the "variable → jq" ergonomic gap without growing
shellcheck-incompatible shell syntax. Preferred over extending
`${VAR.field}` to regular variables: jq stays the one field-access
path, and `<<<` is a generic primitive that pays off beyond jq.

Ordering:

1. **Here-string `<<<`** — generic redirect; natural follow-up to the
   heredoc span-tracking work that just landed (`c21e09c`, `2490127`).
2. **`jq --var NAME`** — narrower. Decide *after* `<<<` lands whether
   `jq -r '.name' <<< "$R"` feels clean enough to skip this.

---

## Part 1 — Here-string `<<<`

### Target behaviour

`cmd <<< word` feeds the expanded value of `word`, plus a trailing
newline, to `cmd`'s stdin. Matches bash.

```
jq -r '.name' <<< "$RESULT"
cat <<< "hello $NAME"
wc -l <<< "$(ls)"
```

### Implementation seams

- **Lexer** — `crates/kaish-kernel/src/lexer.rs`. Add `HereString`
  token; register the three-char pattern with higher priority than
  `<<` so `<<<` doesn't tokenize as `<<` + `<`.
- **AST** — `crates/kaish-kernel/src/ast/types.rs:195`. Add
  `RedirectKind::HereString`. The existing `Redirect { kind, target:
  Expr }` shape already fits — the word is just an `Expr`.
- **Parser** — `crates/kaish-kernel/src/parser.rs` (around L1459
  where redirects are assembled). Parse `<<<` followed by a single
  word/string expression.
- **Validator** — `crates/kaish-kernel/src/validator/walker.rs`.
  Walk the `target` expression like any other redirect target. No new
  rules expected.
- **Interpreter** — `crates/kaish-kernel/src/scheduler/pipeline.rs:29`
  (`apply_redirects`). Expand the target, append `\n`, wire the
  resulting `Vec<u8>` as the command's stdin. Heredoc stdin wiring is
  the reference implementation — follow that path; no temp file.
- **S-expression printer** — `crates/kaish-kernel/src/ast/sexpr.rs`.
  Add a case for the new `RedirectKind` variant so snapshot tests
  render it.

### Tests first (TDD)

**Lexer** (`crates/kaish-kernel/tests/lexer_tests.rs`):
- `<<<` lexes to a single `HereString` token.
- `<<< ` with trailing space still tokenizes cleanly.
- `<<<<` (typo) is an error or tokenizes as `HereString` + `<` —
  pick a behaviour and lock it in. Bash errors; we should too.
- `<<EOF` still tokenizes as `HereDoc`, not `HereString` + trailing
  junk. Confirms priority ordering.

**Parser** (`crates/kaish-kernel/tests/parser_tests.rs` + snapshots):
- `cat <<< "x"` parses with `RedirectKind::HereString` and the
  expected target expression.
- `<<<` with no operand → `ParseError` with span on the `<<<` token.
- Interpolated target: `cat <<< "$VAR"` parses; the target is an
  `Expr::Interpolated` as usual.
- Single-quoted literal: `cat <<< 'raw $VAR'` parses; target is a
  literal string expression.
- Mixed with other redirects: `cat <<< hi > out.txt` parses with
  both redirects on the command.
- S-expression snapshot covers the new variant.

**Interpreter / E2E** (`crates/kaish-repl/tests/integration.rs`):
- `cat <<< hi` → stdout `hi\n`.
- `cat <<< ""` → stdout `\n` (trailing newline still appended).
- `R='{"a":1}'; jq -r '.a' <<< "$R"` → stdout `1`.
- `jq -r '.x' <<< "$(echo '{"x":42}')"` → stdout `42`.
- Pipeline head: `jq '.x' <<< "$J" | wc -l` — here-string feeds
  only the head; pipe wiring unchanged.
- Stdout redirect composes: `cat <<< hi > out.txt` writes `hi\n`.
- Single-quoted target: `cat <<< 'raw $X'` emits `raw $X\n` (no
  expansion).

### Decisions to lock in first

- **Trailing newline:** append `\n` (matches bash).
- **Multiple `<<<` on one command:** reject at parse time with a
  clear error. Kaish prefers loud over silent; bash's "last one
  wins" is a footgun.
- **Word expansion:** reuse existing `Expr` expansion — `"$VAR"`
  expands, `'$VAR'` doesn't. No special-case needed.
- **Interaction with `<`:** `<<<` and `<` on the same command is a
  parse error (same reasoning as two `<<<`).

### Docs

- `docs/LANGUAGE.md` — add `<<<` to the redirection section.
- `crates/kaish-kernel/docs/help/syntax.md` — same.

---

## Part 2 — `jq --var NAME` (conditional on Part 1 feeling insufficient)

### Target behaviour

`jq --var RESULT '.name'` reads `RESULT` from scope and uses its
string value as jq's input, avoiding the `$(… <<< "$R")` subshell.

```
NAME=$(jq -r --var RESULT '.name')   # vs. NAME=$(jq -r '.name' <<< "$RESULT")
```

### Implementation

- `crates/kaish-kernel/src/tools/builtin/jq_native.rs` — add
  `--var` to the schema; in `execute`, look up via
  `ExecContext::scope` and bypass stdin.

### Tests first

`crates/kaish-kernel/tests/jq_tests.rs`:
- Happy path: string variable holding JSON → correct field.
- Unset variable → non-zero exit with a clear error message.
- Variable holding a `Value::Json` (not a string) → serialize back
  to JSON text and feed it. Lock this in a test.
- `--var` *and* stdin both provided → error (ambiguous input).
- `--var` composes with `-r`, multiple filters, etc.

### When to skip

If after Part 1 the idiom `jq -r '.name' <<< "$R"` reads cleanly in
real agent scripts, Part 2 is redundant surface area. Bias toward
not shipping it.

---

## Non-goals

- `${VAR.field}` nested access on regular variables. Skipped — jq +
  `<<<` covers the use case without shellcheck-incompatible syntax.
- Bracket-style JSON access (`${VAR[key]}`). Same reasoning; also
  conflates JSON navigation with bash associative arrays, which
  kaish doesn't have.
- Process substitution `<(cmd)` — already on the explicitly-dropped
  list (`CLAUDE.md`).
