# Builtin Tool Compatibility

Kaish builtins follow the **80/20 rule**: implement the features used 80% of the time, deliberately omit the 20% that add complexity without proportional value.

This document details what each builtin supports and what's intentionally missing compared to GNU, BSD, and POSIX implementations.

## Philosophy

- **Predictable over powerful** — no dark corners or surprising behavior
- **Composable** — missing features can often be achieved by piping to another tool
- **Agent-friendly** — familiar tools are easier for AI to use correctly
- **ERE everywhere** — all regex uses Extended Regular Expressions (like `egrep`), never BRE

---

## Text Processing

### awk

Pattern scanning and text processing language.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Patterns** | `/regex/`, expressions, `BEGIN`, `END`, empty | `BEGINFILE`, `ENDFILE` (gawk) |
| **Fields** | `$0`–`$NF`, `NR`, `NF`, `FS`, `RS`, `OFS`, `ORS` | `FILENAME`, `FNR`, `ARGC`, `ARGV` |
| **Operators** | `+ - * / % ^`, `== != < > <= >=`, `~ !~`, `&& || !`, `= += -= *= /= %=`, `?:`, `++ --` | — |
| **Control** | `if/else`, `while`, `for`, `for-in`, `break`, `continue`, `next`, `exit` | — |
| **Arrays** | Associative access, iteration, `delete` | Multi-dimensional `arr[i,j]` |
| **Functions** | `length`, `substr`, `index`, `split`, `sprintf`, `tolower`, `toupper`, `match` | User-defined functions |
| **I/O** | `print`, `printf` | `getline`, output redirection `>`, `>>`, `|` |
| **Numeric** | — | `sin`, `cos`, `atan2`, `exp`, `log`, `sqrt`, `rand`, `srand` |

**Why no user-defined functions?** Use kaish functions instead.

**Why no getline?** Complex control flow that's error-prone. Read files with `cat` and pipe to awk, or process multiple files separately in kaish.

**Why no output redirection?** Use kaish pipes: `awk '{print $1}' | write output.txt`

```bash
# Kaish awk examples
echo "alice 25\nbob 30" | awk '$2 > 26 {print $1}'     # → bob
awk -F: '{print $1}' /etc/passwd                       # custom separator
awk '{sum += $1} END {print sum}' numbers.txt          # aggregation
awk 'BEGIN {OFS=","} {print $1, $2}' data.txt          # CSV output
```

---

### sed

Stream editor for filtering and transforming text.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Commands** | `s` (substitute), `d` (delete), `p` (print), `q` (quit) | `a`, `i`, `c` (append/insert/change), `y` (transliterate) |
| **Addresses** | Line numbers, `$` (last), `/regex/`, ranges `1,5` `/start/,/end/` | `0` address, step `1~2`, GNU addr extensions |
| **s flags** | `g` (global), `i` (case-insensitive), `p` (print on change) | `n`th occurrence, `w` (write to file), `e` (execute) |
| **Regex** | ERE (extended), capture groups `\1`–`\9`, `&` (whole match) | BRE, GNU extensions `\s`, `\w` |
| **Options** | `-n` (quiet), `-e` (expression) | `-i` (in-place), `-f` (script file), `-z` (null separator) |
| **Other** | — | Hold space (`h`, `H`, `g`, `G`, `x`), branching (`:`, `b`, `t`) |

**Why no in-place editing?** Explicit is better: `sed 's/old/new/' file > file.new && mv file.new file`

**Why no hold space?** The hold space enables multi-line operations but makes sed programs hard to understand. Use awk for complex transformations.

**Why ERE instead of BRE?** Consistency — all kaish regex is ERE. No need to remember which tool uses which syntax.

```bash
# Kaish sed examples
sed 's/foo/bar/' file.txt                    # first occurrence per line
sed 's/foo/bar/g' file.txt                   # all occurrences
sed '/error/d' log.txt                       # delete matching lines
sed -n '/pattern/p' file.txt                 # print only matches
sed '2,5d' file.txt                          # delete line range
sed 's|/usr|/opt|g' file.txt                 # alternative delimiter
sed 's/(\w+) (\w+)/\2, \1/' names.txt        # capture groups
```

---

### grep

Search file contents for patterns.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Matching** | ERE patterns, `-i` (case-insensitive), `-v` (invert), `-w` (word) | BRE, Perl regex `-P` |
| **Output** | `-l` (files only), `-c` (count), `-n` (line numbers), `-o` (only matching), `-q` (quiet) | `--color` |
| **Context** | `-A`, `-B`, `-C` (after/before/context lines) | — |
| **Input** | Files, stdin, `-r`/`-R` (recursive) | `-z` (null separator) |
| **Filtering** | `--include`, `--exclude` (glob patterns for recursive) | — |

```bash
grep "error" log.txt                         # basic search
grep -i "error" log.txt                      # case-insensitive
grep -v "debug" log.txt                      # invert match
grep -c "pattern" file.txt                   # count matches
grep -n "TODO" *.rs                          # with line numbers
grep -rn "TODO" src/                         # recursive with line numbers
grep -r "pattern" . --include="*.rs"         # recursive, Rust files only
```

---

### cut

Extract fields or character ranges from lines.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Modes** | `-f` (fields), `-c` (characters) | `-b` (bytes), multiple files |
| **Options** | `-d` (delimiter) | `--complement`, `--output-delimiter` |
| **Ranges** | `N`, `N-M`, `N-`, `-M` | — |

**Note:** `cut` processes a single file or stdin. Multiple file arguments are not supported (only the first file is used).

```bash
cut -f1 -d: /etc/passwd                      # first field, colon delimiter
cut -f1,3 -d, data.csv                       # multiple fields
cut -c1-10 file.txt                          # first 10 characters
```

---

### tr

Translate or delete characters.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Operations** | Translate, `-d` (delete), `-s` (squeeze) | `-c` (complement) |
| **Classes** | `[:alpha:]`, `[:digit:]`, `[:space:]`, etc. | — |

```bash
echo "hello" | tr 'a-z' 'A-Z'                # uppercase
echo "hello   world" | tr -s ' '             # squeeze spaces
echo "hello123" | tr -d '0-9'                # delete digits
```

---

### sort

Sort lines of text.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Order** | `-r` (reverse), `-n` (numeric) | `-V` (version), `-h` (human numeric) |
| **Keys** | `-k` (field key), `-t` (separator) | — |
| **Options** | `-u` (unique) | `-s` (stable), `-m` (merge) |

```bash
sort file.txt                                # alphabetical
sort -n numbers.txt                          # numeric
sort -k2 -t: /etc/passwd                     # by second field
sort -u file.txt                             # unique lines
```

---

### uniq

Filter adjacent duplicate lines.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Options** | `-c` (count), `-d` (duplicates only), `-u` (unique only) | `-i` (case-insensitive), field/char skipping |

**Note:** Input should be sorted first for meaningful results.

```bash
sort file.txt | uniq                         # remove duplicates
sort file.txt | uniq -c                      # count occurrences
sort file.txt | uniq -d                      # show only duplicates
```

---

### wc

Count lines, words, and characters.

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Counts** | `-l` (lines), `-w` (words), `-c` (chars) | `-m` (chars vs bytes distinction) |

```bash
wc -l file.txt                               # line count
wc -w file.txt                               # word count
cat file.txt | wc -l                         # from stdin
```

---

## JSON

### jq

JSON query and transformation (powered by native jaq implementation).

| Category | Supported | Deliberately Omitted |
|----------|-----------|---------------------|
| **Queries** | `.field`, `.[n]`, `.[]`, pipes, `map`, `select`, `type`, `length` | User-defined functions, `import` |
| **Options** | `-r` (raw output), `-c` (compact) | `-S` (sort keys), `--slurpfile` |
| **Builtins** | Standard jq library via jaq-std | Some niche functions |

This is a full jq implementation via [jaq](https://github.com/01mf02/jaq) — most jq filters work as expected.

```bash
echo '{"name":"alice"}' | jq '.name'                    # → "alice"
echo '{"name":"alice"}' | jq -r '.name'                 # → alice (no quotes)
echo '[1,2,3]' | jq '.[0]'                              # → 1
echo '[1,2,3]' | jq 'map(. * 2)'                        # → [2,4,6]
echo '[{"a":1},{"a":2}]' | jq '[.[] | select(.a > 1)]' # → [{"a":2}]
jq '.items[]' /path/to/file.json                        # read from file
```

---

## Files & I/O

### ln / readlink

Symbolic link tools.

| Tool | Supported | Deliberately Omitted |
|------|-----------|---------------------|
| `ln` | `-s` (symbolic), `-f` (force) | Hard links |
| `readlink` | Raw symlink target, `-f` (canonicalize) | `-e`, `-m` (canonicalize modes) |

**Why no hard links?** The VFS abstraction doesn't support hard links. Use symlinks instead.

**Note:** `ls` shows symlinks with an `@` suffix (short format) and `name -> target` (long format), similar to `ls -F`.

```bash
ln -s /path/to/target link_name     # create symlink
ln -sf target.txt link.txt          # force replace existing link
readlink link.txt                   # show symlink target
readlink -f ../some/./path          # canonicalize path
ls -la /path                        # shows: link.txt -> target.txt
```

---

### cat / head / tail

File content output tools.

| Tool | Supported | Deliberately Omitted |
|------|-----------|---------------------|
| `cat` | Read and concatenate files, `-n` (line numbers) | `-s` (squeeze blank), `-A` (show all) |
| `head` | `-n` (lines), `-c` (chars) | Multiple files |
| `tail` | `-n` (lines), `-c` (chars) | `-f` (follow), multiple files |

**Note:** `-c` counts UTF-8 characters, not bytes (deliberate simplification for Unicode correctness).

**Note:** `head` and `tail` process a single file or stdin. Multiple file arguments are not supported (only the first file is used).

**Why no `tail -f`?** Use a dedicated log-watching tool or MCP integration for real-time streams.

---

## Summary Table

| Tool | Scope | Key Omissions | Rationale |
|------|-------|---------------|-----------|
| **awk** | 80% | User functions, getline, output redir | Use kaish functions, pipes |
| **sed** | 70% | Hold space, branching, in-place | Use awk for complex cases |
| **grep** | 95% | Perl regex, `--color` | ERE is enough |
| **cut** | 90% | Byte mode, complement | Rarely needed |
| **tr** | 85% | Complement | Use sed for complex cases |
| **sort** | 80% | Version sort, stable | Specialized needs |
| **uniq** | 80% | Case-insensitive, field skip | Pre-process with awk |
| **jq** | 70% | Advanced filters, modules | Native jaq covers common transforms |

---

## See Also

- [README.md](../README.md) — Project overview and quick tour
- [LANGUAGE.md](LANGUAGE.md) — Language reference
