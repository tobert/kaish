# 散/集 (Scatter/Gather) — Parallel Map over Structured Data

## Syntax

```
input | scatter [--as VAR] [--limit N] [--timeout DUR] | worker… | gather [--lines|--json]
```

## The model

scatter fans structured input out to parallel workers; gather collects one
**JSONL result record per worker**, in item order, **every worker — failures
included**:

```jsonl
{"i":0,"item":"web-1","ok":true,"code":0,"out":"restarted","err":""}
{"i":1,"item":"web-2","ok":false,"code":7,"out":"","err":"connection refused"}
```

Fields: `i` (item index), `item` (the input, **typed** — a record stays a
record), `ok`, `code`, `out` (worker stdout, trailing newline stripped), `err`
(worker stderr — always present) on every row; `data` (the worker's structured
output, typed) and `timed_out:true` only when present. A timed-out worker
reports `code` 124.

`ITEM` is **typed**: from a JSON array (jq/`values`/seq/split/glob/find), each
worker binds the real element — `${ITEM[id]}` subscripts a record, numbers stay
numbers, `1` ≠ `"1"`. From plain text, one string item per line (blank lines
skipped). Quote it at command boundaries: `work "$ITEM"` (a record renders as
compact JSON). A single non-array object, a `null` element, or binary input is
a loud error.

## Consuming results

```sh
# kaish jq gets the records as ONE ARRAY — stream rows with .[] :
… | gather | jq -r '.[] | select(.ok) | .out'        # successes' outputs
… | gather | jq -r '.[] | select(.ok | not) | .item' # failed items
… | gather | jq '[.[] | select(.ok)] | length'       # count successes

# Typed iteration — each r is a record:
for r in $(… | gather); do
  if [[ ${r[ok]} == false ]]; then retry "${r[item]}"; fi
done

… | gather --json    # same records as one pretty JSON array
… | gather --lines   # raw successful outputs only, item order — HARD ERROR
                     # (exit 123, no partial text) if ANY worker failed
```

## Exit codes

`0` every worker succeeded · `123` any worker failed (partial or total —
distinguish via the rows) · `2` usage error. A pipeline's status is its last
command's, so check gather's code directly or gate with `if`/`&&`.

## Parameters

**scatter:** `--as VAR` (default `ITEM`) — binding name per item. `--limit N`
(default 8, clamped 1..=10000) — max concurrent workers. `--timeout DUR`
(`30`, `5s`, `500ms`, `2m`, `1h`) — per-worker timeout; cancels the worker,
kills its external children, row gets `code` 124 + `timed_out:true`.

**gather:** `--lines` — raw text escape hatch (above). `--json` — one JSON
array instead of JSONL rows.

## Examples

```sh
# Typed records from a file: deploy each job, then list timeouts
jq '.jobs' jobs.json | scatter --limit 4 --timeout 30s \
  | deploy --id "${ITEM[id]}" --host "${ITEM[host]}" \
  | gather | jq -r '.[] | select(.timed_out) | .item.id'

# Fan out an existing collection variable
values $hosts | scatter | ping -c1 "$ITEM" | gather

# Sum parallel outputs (out is a string — tonumber)
seq 1 20 | scatter | slowsquare "$ITEM" | gather --json \
  | jq '[.[] | .out | tonumber] | add'

# Retry failures sequentially
for r in $(cat hosts.txt | scatter --as H | probe "$H" | gather); do
  if [[ ${r[ok]} == false ]]; then probe --slow "${r[item]}"; fi
done
```

## Behavior

- **Rows are in item order** (deterministic), regardless of completion order.
- Each worker runs the pipeline segment in its own **forked kernel**
  (snapshotted scope/cwd/aliases/user tools) — full dispatch chain works;
  worker mutations don't leak back. A worker's `code` is its segment's last
  command's exit code.
- Workers all run to completion — gather never short-circuits remaining
  workers on a failure (contrast with POSIX `xargs`, which may stop early).
- Nested scatter: pass `--as` explicitly — an inner default `ITEM` shadows the
  outer binding.
- **Cancellation cascades**: parent timeout / `Kernel::cancel` / Ctrl-C
  propagates into every worker (SIGTERM → `kill_grace` → SIGKILL).
- Empty input: exit 0, no rows.
