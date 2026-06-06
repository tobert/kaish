# rg — Recursive Search (ripgrep)

Recursively searches paths for a regex pattern. Backed by ripgrep's
`grep-searcher` + `grep-regex` engine, with kaish's VFS plumbing so
searches across `/v/...` mounts work the same as searches on disk.

## Quick start

```bash
rg pattern .                  # search recursively from cwd
rg -i hello src/              # case-insensitive
rg -trust 'fn main'           # only Rust files
rg -Tjs TODO                  # skip JS files
rg -U '(?s)foo.*bar' src/     # multiline pattern
rg --hidden secret             # include hidden files
rg --files .                  # list searchable files (no search)
echo 'hi foo' | rg foo        # search stdin
```

## Backend dispatch

- **Real-FS roots** — uses ripgrep's `ignore::WalkBuilder`. You get
  parent-directory `.gitignore` walk-up, type filters with rg's full
  ~100 built-in types, hidden-file handling, `.git/info/exclude`,
  global gitignore, and ripgrep's correct ignore-precedence semantics.
- **VFS roots** (e.g. `/v/git/<repo>/...` or `/v/mem/...`) — uses
  kaish's own `FileWalker` with the same gitignore + type filtering
  semantics that Pillar C of the rg-builtin plan landed.

## Output

Default text mode renders one line per match in `path:line:content`
form, matching grep. Under the global `--json` flag, each match
becomes a structured object:

```json
{
  "path": "src/main.rs",
  "line_number": 42,
  "byte_offset": 1234,
  "line_text": "    println!(\"hello\");",
  "submatches": [
    { "text": "println", "start": 4, "end": 11 }
  ]
}
```

`grep --json` returns the same shape. Use `kaish-jq` to filter.

## Common flags

| Flag | Meaning |
|------|---------|
| `-i` | Case-insensitive |
| `-w` | Match whole words |
| `-F` | Treat pattern as a literal |
| `-v` | Invert match |
| `-n` / `-N` | Show / suppress line numbers |
| `-c` | Per-file match count |
| `-l` | Filenames with matches only |
| `-o` | Print only matched substrings |
| `-A`/`-B`/`-C N` | Context lines |
| `-m N` / `--max-count` | Stop after N matches per file |
| `--max-depth` | Limit directory recursion |
| `--max-filesize` | Skip files above N bytes |
| `-U` | Multiline matches |
| `-t TYPE` / `--type` | Only files of TYPE (rust, js, py, …) |
| `-T TYPE` / `--type-not` | Skip files of TYPE |
| `--hidden` | Include hidden files |
| `--no-ignore` | Don't honor `.gitignore` / `.ignore` / `.rgignore` |
| `--include` / `--exclude` | Glob-based filename filters |
| `--encoding LABEL` | Force `utf-16`, `latin-1`, etc. |
| `--binary MODE` | `quit` (default), `text`, `without-match` |
| `--files` | List searchable files without searching |
| `-P` | PCRE2 regex (requires `--features pcre2`) |

## Caveats

- Parallel walking is currently sequential — the speed is competitive
  with stand-alone rg on small/medium trees, falls behind on very
  large trees. See `docs/issues.md`.
- WASI builds aren't supported yet (the `grep-searcher` and `ignore`
  crates pull in `memmap2` / `walkdir` which don't compile on
  `wasm32-wasip1`). Use the `native` feature, which is the default.
