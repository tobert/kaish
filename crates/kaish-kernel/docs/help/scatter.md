# 散/集 (Scatter/Gather) — Parallel Processing

## Syntax

```
input | scatter [as=VAR] [limit=N] | command | gather [first=N] [format=lines|json]
```

## Parameters

**scatter:** `as=VAR` (default: `ITEM`) — variable name per item. `limit=N` (default: 8) — max concurrent workers.

**gather:** `first=N` (default: 0/all) — take first N results. `format=lines|json` (default: `lines`) — output format.

## Example

```bash
# Fan out to 4 workers, compute squares, collect first 5 as JSON
seq 1 20 | scatter as=N limit=4 | echo "{\"id\": $N, \"square\": $((N * N))}" | gather first=5 format=json

# Process files in parallel
glob "*.json" | scatter as=FILE limit=4 | jq ".name" $FILE | gather
```

## Behavior

- Results arrive in **completion order**, not input order
- Each worker runs the full pipeline with `$VAR` set to its item
- Outer scope variables are visible to workers
- Workers share the same VFS
- Failed workers: error collected, other workers continue
- `set -e` before scatter stops on first error
