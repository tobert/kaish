# Parallel Map with Scatter and Gather

## Syntax

```sh
input | scatter [--as VAR] [--limit N] [--timeout DUR] | worker… | gather [--lines|--json]
```

## Output Model

`scatter` fans structured input out to parallel workers. `gather` collects one JSONL result record per worker in item order, including failures.

```jsonl
{"i":0,"item":"web-1","ok":true,"code":0,"out":"restarted","err":""}
{"i":1,"item":"web-2","ok":false,"code":7,"out":"","err":"connection refused"}
```

| Field | Presence | Meaning |
|---|---|---|
| `i` | Always | Zero-based item index |
| `item` | Always | Typed input item |
| `ok` | Always | Boolean worker success status |
| `code` | Always | Worker exit code, or `124` on timeout |
| `out` | Always | Worker `stdout` with trailing newline stripped |
| `err` | Always | Worker `stderr` |
| `data` | When present | Typed structured output from the worker |
| `timed_out` | When timed out | `true` when the worker exceeds `--timeout` |

Check `ok` instead of `code`. A worker with binary `stdout` gets `ok:false`, `code` `0`, and an error message in `err`.

## Typed ITEM Binding

- **JSON arrays** from `jq`, `values`, `seq`, `split`, `glob`, or `find` bind each real element to `ITEM`.
- **Types and fields** stay intact. `${ITEM[id]}` reads a record field, and `1` stays a number instead of `"1"`.
- **Plain text** splits into one string item per line and skips blank lines.
- **Command boundaries** require quotes around `"$ITEM"`. Records render as compact JSON.
- **External JSONL streams** require `fromjsonl` first, such as `curl … | fromjsonl | scatter …`. See `help fromjsonl`.
- **Invalid inputs** fail loudly. Single non-array objects, `null` elements, and binary inputs produce an error.

## Consuming Results

```sh
# kaish jq receives all records as one array, so stream rows with .[]
… | gather | jq -r '.[] | select(.ok) | .out'        # successful outputs
… | gather | jq -r '.[] | select(.ok | not) | .item' # failed items
… | gather | jq '[.[] | select(.ok)] | length'       # count successes

# Iterate over typed records
for r in $(… | gather); do
  if [[ ${r[ok]} == false ]]; then retry "${r[item]}"; fi
done

… | gather --json    # emit one JSON array instead of JSONL
… | gather --lines   # emit raw successful outputs, exits 123 with no output on failure

# Round-trip JSONL rows through a file
… | gather > results.jsonl
cat results.jsonl | fromjsonl | jq '.[] | select(.ok | not)'
cat results.jsonl | fromjsonl | scatter | retry "${ITEM[item][host]}" | gather
```

## Exit Codes

| Exit code | Meaning |
|---|---|
| `0` | Every worker succeeded, or input was empty |
| `123` | At least one worker failed, or `--lines` saw a failure |
| `2` | Usage error |

A pipeline returns its last command's exit code. An aggregate `123` means at least one row has `ok:false`. Each row's `code` holds that worker's own exit code.

## Parameters

| Command | Flag | Default | Behavior |
|---|---|---|---|
| `scatter` | `--as VAR` | `ITEM` | Variable name bound to each worker item |
| `scatter` | `--limit N` | `8` | Maximum concurrent workers, clamped to `1..=10000` |
| `scatter` | `--timeout DUR` | none | Per-worker timeout such as `30`, `5s`, `500ms`, `2m`, or `1h` |
| `gather` | `--lines` | off | Emit raw successful outputs, or exit `123` with no text on failure |
| `gather` | `--json` | off | Emit one JSON array instead of JSONL rows |

When `--timeout` expires, `scatter` cancels the worker, kills its external children, and sets `code` `124` and `timed_out:true`.

## Examples

```sh
# Deploy typed job records and list timeouts
jq '.jobs' jobs.json | scatter --limit 4 --timeout 30s \
  | deploy --id "${ITEM[id]}" --host "${ITEM[host]}" \
  | gather | jq -r '.[] | select(.timed_out) | .item.id'

# Fan out an existing collection variable
values $hosts | scatter | ping -c1 "$ITEM" | gather

# Sum parallel outputs
seq 1 20 | scatter | slowsquare "$ITEM" | gather --json \
  | jq '[.[] | .out | tonumber] | add'

# Retry failures sequentially
for r in $(cat hosts.txt | scatter --as H | probe "$H" | gather); do
  if [[ ${r[ok]} == false ]]; then probe --slow "${r[item]}"; fi
done
```

## Behavior

- **Deterministic order:** `gather` emits rows in input item order regardless of worker completion order.
- **Isolated workers:** Each worker runs in a forked kernel with snapshotted scope, cwd, aliases, and user tools. Worker mutations never leak back.
- **Worker exit code:** Each row's `code` comes from the last command in that worker's pipeline segment.
- **Complete execution:** `gather` runs every worker to completion and collects every result even when earlier workers fail.
- **Nested scatter:** Pass `--as` explicitly in inner `scatter` stages so an inner `ITEM` does not shadow the outer binding.
- **Cascading cancellation:** Parent timeouts, `Kernel::cancel`, and Ctrl-C send `SIGTERM`, wait for `kill_grace`, and send `SIGKILL` to every worker.
- **Empty input:** Empty input emits no rows and exits `0`.
