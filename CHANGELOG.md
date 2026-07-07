# Changelog

All notable changes to **kaish** (会sh) are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

While kaish is pre-1.0, minor (`0.X.0`) releases may carry breaking changes;
breaking entries are marked **BREAKING**.

## [Unreleased]

### Fixed
- **`glob --include` now actually filters.** It was a complete no-op: the
  walker consulted only exclude rules, so `glob '*' --include='*.rs'` listed
  everything. Include semantics are now rg-like: when include patterns exist a
  file must match one of them (by relative path or basename); directories are
  still traversed so included files below them are reached; exclude patterns
  still prune whole subtrees. (`grep --include` half-worked through a separate
  walk-pattern hack, now removed in favor of the same filter.)
- **Repeated `--include`/`--exclude` accumulate in `glob` and `grep`.** Both
  were bound single-valued, so repeating the flag silently kept only the LAST
  pattern — `glob` while its help said "can be repeated", and `grep -r TODO .
  --include='*.rs' --include='*.toml'` silently searched only the toml files
  (a false negative). Repeats now accumulate like `--ftype` always has.
- **A malformed numeric flag value errors instead of silently meaning
  "unlimited"/"disabled".** `glob --depth=abc`, `tree -L abc`, and
  `find -maxdepth xyz` all exited 0 and walked without a depth limit;
  `spawn timeout=abc` silently DISABLED the timeout (unbounded child);
  `split --limit` and `head -c` wrapped negative values through `usize`.
  All now fail loudly, and negative values are refused rather than wrapped.
  `glob --type=<unknown>` similarly errored silently into "files only" and is
  now a usage error.
- **`spawn`'s own help examples didn't work.** `help spawn` taught
  `spawn command="cargo" argv=["build"]`, but that word-assign form never
  binds for spawn — the whole token became the program name and exited 127.
  The examples now show the working flag form (`spawn --command cargo --argv
  build`).
- **Unquoted `glob **/*.rs` now works: the pattern reaches the builtin as
  written.** Previously the kernel's argv glob expansion pre-expanded the bare
  pattern into matching paths, so `glob` bound the first path as its "pattern",
  silently ignored the rest, and printed exactly one file — after walking the
  tree twice. The builtin's own examples (and agents following them) spell the
  pattern unquoted. Quoted patterns behave as before.

### Added
- **`ToolSchema::glob_passthrough` (+ `with_glob_passthrough()`)** — a tool
  whose input *is* a glob pattern (like `glob`) can now tell the argv binder to
  pass bare patterns through as literal text instead of expanding them.
  Embedder tools with pattern-shaped inputs can opt in the same way.
- **`glob` accepts multiple patterns** (`glob **/*.rs **/*.toml`): matches are
  the deduped union in pattern order; any pattern with zero matches fails the
  whole command (exit 1) naming the pattern that missed.
- **A backgrounded confirmation latch is now surfaced and fulfillable** (GH
  #96). A destructive op gated under `set -o latch` and run in the background
  (`rm x &`) stored its `LatchRequest` but no consumer exposed it, so the nonce
  was unreachable and the gate could never be confirmed. Now: `wait` surfaces
  the request on the result's `.latch` field (exit 2, like a foreground gate);
  `jobs` and `/v/jobs/{id}/status` report `Latched` distinctly from a plain
  failure; and a new `/v/jobs/{id}/latch` node renders the request as JSON
  (nonce, command, paths, hint) — empty when the job isn't gated. An embedder
  fulfills it with `Kernel::confirm(&latch)`. **Embedders:** `JobStatus` gains
  a `Latched` variant (exhaustive matches must handle it) and `JobInfo` gains
  a `latch: Option<LatchRequest>` field.
- **Recursion is depth-guarded (`MAX_RECURSION_DEPTH` = 48)** (GH #46/#47, tuned
  by #48). Command substitution, shell-function calls, `.kai` script execution,
  and `source`/`.` all re-enter the statement engine on the native stack; a
  runaway or mutually recursive script now returns a loud `maximum recursion
  depth exceeded` error instead of overflowing the stack (a `SIGSEGV`/abort with
  no diagnostic). Two new `pub const`s let embedders size their runtime:
  `MAX_RECURSION_DEPTH` and the paired `RECOMMENDED_STACK_SIZE` (12 MiB) — the
  cap trips before `cap × per-level-stack` can exceed the floor. #48's smaller
  interpreter frames let the cap rise (32→48) and the floor drop (16→12 MiB)
  together while keeping the same safety margin. The guard only fires *before* an
  overflow on a thread that meets that floor — the reference REPL now sizes its
  tokio worker threads (`thread_stack_size`) and its `block_on` driver thread to
  it; embedders should too (see `docs/EMBEDDING.md`).

### Changed
- **Interpreter allocation/stack pass** (GH #48). The native stack consumed per
  statement-engine re-entry level (`$(…)`, shell functions, `source`) is cut
  ~46% (release ~92 → ~50 KB/level; debug similarly), shrinking the interpreter's
  hot futures by boxing the cold dispatch branches, the per-command
  `ExecContext`/scope snapshots, and `ExecResult`'s structured-output field, and
  by dropping the per-command tracing spans on the recursion ring. The two
  interpreter crates (`kaish-kernel`, `kaish-types`) also build at
  `opt-level = 1` in dev/test now (contained compile-time cost, ~7× smaller debug
  stack). A new `recursion_stack_cost_tests` probe measures the per-level cost.
  This leaves headroom to raise `MAX_RECURSION_DEPTH` or lower
  `RECOMMENDED_STACK_SIZE` as a follow-up (GH #46/#47).
- **Removed the internal per-command/per-pipeline tracing spans** from the hot
  execution ring (GH #48) — coarse spans remain on the outer execute entries, so
  embedders consuming kaish's `tracing` output now see one execution span per
  top-level call rather than one per nested command. Observability-shape change,
  not a behavior change.
- **BREAKING (embedders):** `ExecContext.tool_schemas` is now `Arc<[ToolSchema]>`
  instead of `Vec<ToolSchema>` (GH #48) — the ~70-entry schema catalog is shared
  by refcount rather than deep-cloned into every per-command context.
  `set_tool_schemas` still accepts a `Vec` (converts internally) and all read
  sites are unaffected (deref coercion to `&[ToolSchema]`); only direct field
  assignment/mutation of the public field needs `.into()`.
- **Embedders:** `JobStatus`, `JobInfo`, and `ToolResult` are now
  `#[non_exhaustive]` (GH #93, part of item 3/4 plus a hygiene pass) —
  construct them via their constructors (`JobInfo::new()` +
  `.with_output_file()`/`.with_pid()`/`.with_latch()`; `ToolResult::success()`/
  `.failure()`/`.with_data()` plus the new `.with_output()`/
  `.with_content_type()`/`.with_baggage()`/`.with_latch()`/`.with_did_spill()`/
  `.with_original_code()`) and add a `_` arm to any exhaustive match — future
  variants/fields won't break you.

### Fixed
- **A backgrounded confirmation latch can no longer be destroyed silently**
  (GH #96 follow-up). `jobs --cleanup` used to reap a latched job like any
  completed one, and `kill %N` removed it outright — both silently dropped the
  stored `LatchRequest`, leaving the gated operation permanently
  unconfirmable. `jobs --cleanup` now keeps latched jobs and says so; `kill
  %N` on a latched job refuses with a pointer to the nonce; the new `kill
  --discard %N` abandons the gate explicitly and loudly (the gated operation
  never runs).
- **README install instructions pointed at a crate that doesn't exist.**
  `cargo install kaish` fails — there is no `kaish` package on crates.io; the
  binary named `kaish` ships in the `kaish-repl` crate. The README now says
  `cargo install kaish-repl` (and was restructured for first-time visitors
  alongside; the exit-code table and output contract moved to
  `docs/EMBEDDING.md`, trash thresholds to `docs/LANGUAGE.md`).
- **`jq -s`/`--slurp` now wraps the `.data` pipeline path in an array-of-one,
  matching real jq** (GH #93 item 2). Real `jq -s` always wraps its input in
  an array, even a single document. On kaish's structured `.data` shortcut
  (a scalar or record handed over by an upstream stage like `fromjson`),
  `-s` was a no-op, so `<produces scalar .data> | jq -s length` diverged from
  real jq. It now wraps the incoming value in a one-element array before
  applying the filter, same as the text path; plain `jq` (no `-s`) on the
  `.data` path is unchanged.
- **Deep recursion no longer crashes the process** (GH #46). `f() { f; }; f`,
  mutual recursion, and deeply nested `$(...)` aborted with a bare stack
  overflow; they now hit the depth guard above and fail loudly.
- **`$(...)` in a redirect target now works on a bare `Kernel::execute`** (GH
  #90). Command substitution in a redirect target or heredoc body (`echo x >
  $(gen)`, `cat < $(gen)`, `cmd > $(gen)` in a pipeline stage) only ran when the
  kernel was Arc-attached via `into_arc` — the REPL. A bare `Kernel::execute`
  (every embedder holding a `Kernel` by value, and the whole test harness) left
  `ctx.dispatcher` unset, so the target silently fell back to a sync evaluator
  that can't run `$()` and failed with "could not evaluate redirect target".
  The redirect evaluator now takes the dispatcher the runner already holds, so
  the behavior no longer depends on how the kernel was constructed.
- **Binary (`Value::Bytes`) now goes loud at every remaining text sink** (GH
  #93 item 1), not just the primary ones 0.11.0 fixed. A path-coercing builtin
  positional (`mkdir`/`cp`/`rm`/`ls`/`find`/`grep`/`sed`/`uniq`/`jq`/`tree`/
  `write`/`ln`/`patch`/`checksum`/`cmp`/`spawn`/`cd`/`awk`/etc.), a
  `[[ -f $x ]]`/`test -f $x` file-test path, an exported env var reaching a
  spawned process, a redirect target (`cmd > $x`), a `case $x in`/`==`/`in`
  operand, and `exec`'s own argv all used to silently mishandle a binary
  operand instead of erroring — most stringified it into the `[binary: N
  bytes]` placeholder (a wrong path, a wrong env var value, a comparison
  against placeholder text instead of the real bytes), and a few (`cat`/
  `head`/`tail`/`wc`/`sed`/`uniq`/`jq`/`cd`/`awk`) instead silently dropped it
  and fell back to reading stdin or `$HOME`. All now error clearly instead.
- **`&>` (`RedirectKind::Both`) streams structured output like `>`/`>>`** (GH
  #93 item 6) instead of building the whole output as one `String` first —
  aligns it with the lazy `take_output_for_stream`/`write_canonical` path the
  plain stdout redirects already use.
- **`grep -r PATTERN FILE`** (a file operand, not a directory) now searches
  that file instead of silently finding nothing (GH #105). `-r`/`-R` used to
  treat every operand as a walk root; a file has nothing "under" it, so the
  walk collected zero entries → 0 matches, exit 1, no error — a false negative
  a model reads as "not found". `-r` now governs only how *directories* expand:
  files are searched directly, directories walked, and a mixed `grep -r p file
  dir` operand list does both.
- **A backslash-escaped quote in a `${VAR:-default}` default word no longer
  corrupts the value** (GH #93 item 5). `${UNSET:-"hello \"world\""}` used to
  toggle quote-tracking state on the escaped inner `"` (any `"`/`'` flipped
  state regardless of a preceding `\`), mangling the default to `hello
  \world\`. Escape handling now tracks context, matching bash: outside any
  quotes both `\"` and `\'` unescape (so the `'it'\''s'` → `it's` embedding
  idiom resolves); inside double quotes only `\"` unescapes while `\'` stays
  literal (`"a\'b"` → `a\'b`, since `'` is an ordinary character there); and
  single-quoted default words remain a fully literal region (zero escape
  processing, zero interpolation).
- **`ToolResult` no longer drops `did_spill`/`original_code` crossing the
  backend seam** (GH #93 item 3). `ExecResult` already tracked whether the
  output limiter capped a result and its pre-spill exit code; `ToolResult` had
  neither field, so a backend-registered tool's (kaijutsu, an MCP engine)
  capped result silently looked uncapped by the time it reached the kernel, in
  both `ExecResult`↔`ToolResult` directions. Both fields now round-trip intact.

## [0.11.0] - 2026-07-04

### Added
- **`test` builtin** — POSIX condition evaluation as a command, following
  kaish's `[[` semantics (exit `0` true / `1` false / `2` usage-or-type error).
  File tests (`-e -f -d -r -w -x`) stat through the VFS, not the host FS; string
  equality (`= == !=`) is literal; numeric (`-eq -ne -gt -lt -ge -le`) uses
  kaish (JSON) number semantics, so floats compare (identical to `[[`) and a
  non-numeric operand is loud. Negation is a single leading `!`. Deliberately
  **no `-a`/`-o`/`( )`** (rejected loudly — chain with shell `&&`/`||` or use
  `[[ ]]`) and **no arg-count magic** (an operator missing its operand is a loud
  error, not a surprise-true). Replaces the previous shell-out to `/usr/bin/test`.
  Membership/regex/shape-guards remain `[[ ]]`-only.
- **`ToolSchema.raw_argv`** (embedder API) — a tool can opt into receiving its
  argv in source order with `Value` types preserved (no flag/positional split),
  for position-sensitive commands whose operands may look like flags (`test $x =
  -n`, `test 0 -gt -5`). Used by the `test` builtin.
- **The confirmation latch is now a first-class typed field with a
  fulfillment API** (GH #92). A gated destructive op returns exit 2 with the
  request on a dedicated `ExecResult.latch: Option<Box<LatchRequest>>`
  (control-plane), no longer serialized into the data-plane `.data`. Read it via
  `ExecResult::latch_request()` (unchanged); fulfill it via the new
  `Kernel::confirm(&LatchRequest)`, which replays the **exact captured argv**
  (`LatchRequest.tool` + `.argv`, new fields) with `--confirm=<nonce>` — precise
  even for paths with spaces/globs, where the human `hint` string can't round
  trip. **BREAKING (embedders/`--json`):** a latched `--json` result now surfaces
  the nonce under a `latch` key in the error envelope, not nested under `data`;
  embedders reading the nonce out of `.data` must switch to `latch_request()`.
- **Redirects now work inside `$(...)`** — `x=$(cmd > file)`, `$(cmd 2> err)`,
  `$(cmd >> log)` and friends parse and run, matching the top-level command
  grammar (the command-substitution body used to reject any redirect). Control
  structures inside `$()` remain out of scope.
- **`fromjsonl` / `tojsonl`** — the JSONL (NDJSON) doors, GH #80. `fromjsonl`
  parses strictly line-oriented JSONL text (one JSON document per line, blank
  lines skipped, any bad line — including a truncated trailing one — a loud
  error naming the line number; empty input is a legitimate zero-document
  stream → `[]`) into a typed list in `.data`. `tojsonl` is the egress mirror:
  a list in, one compact JSON document per line out (a record/scalar is one
  document — that's `tojson`'s job). Unlocks `curl … | fromjsonl | scatter`
  and re-ingesting kaish's own `gather` output (`cat results.jsonl | fromjsonl
  | jq …`).

### Changed
- **Comparison/negation operators reach commands as literal argv.** The command
  grammar now accepts `=`, `==`, `!=`, and `!` as literal positional words, so
  POSIX `test a = b` / `test ! -f x` reach the `test` command. As a side effect,
  a *spaced* `cmd key = value` now parses as `cmd` with three arguments (like
  bash) instead of a parse error; a *glued* `key=value` is still a shell
  assignment. The angle brackets `< > <= >=` remain redirection (unchanged).
- **Parser dependency migrated off a dead-ended prerelease**: chumsky
  `1.0.0-alpha.8` → `0.13` (GH #98). Zero code changes — kaish's combinator
  surface survived the upstream stabilization intact (verified by the full
  gate suite passing unchanged, snapshots included). Embedders see a smaller
  dependency tree (two stale transitive regex crates drop out).
- **`jq -s`/`--slurp` is now real jq slurp semantics**, not a no-op (GH #80).
  On the text path it parses the whole whitespace-separated document stream
  and always wraps the result in an array — even a single document
  (`printf '{"a":1}' | jq -s length` → `1`, the array length, not a no-op's
  accidental key count). On the `.data` pipeline path `-s` stays a no-op (the
  upstream stage already handed over one structured value). Plain `jq` (no
  `-s`) on JSONL-shaped text now names the document count and points at
  `fromjsonl`/`jq -s` instead of a bare "trailing characters" parse error; the
  runtime `cannot index [...] with ...` error gains a `.[] | …` hint (kaish
  jq is always-slurped).
- **BREAKING: scatter/gather is now the typed parallel map** (GH #73,
  panel-validated). `gather` emits one JSONL result record per worker, in item
  order, failures included — `{"i":N,"item":<typed>,"ok":…,"code":…,"out":…,
  "err":…}` plus `data`/`timed_out` when present (timeout → `code` 124). The
  same records are the structured data (`for r in $(… | gather)` iterates typed
  records; kaish `jq` receives them as one array — stream rows with `.[]`), and
  `gather --json` renders one JSON array. Worker bindings are TYPED: a JSON
  array fans out element-by-element with real types (`${ITEM[id]}` subscripts a
  record; `1` and `"1"` no longer conflate). Exit codes: `0` all ok · `123` any
  worker failed · `2` usage. `gather --format` and `--first` are REMOVED;
  new `gather --lines` emits raw successful outputs and hard-errors if any
  worker failed. Scatter ingress errors loudly on a single non-array object
  (with a select-the-array hint), `null` items, and binary input; blank text
  lines are skipped. A future `xargs` builtin (GH #78) will carry the POSIX
  bare-lines flavor.

### Changed
- **BREAKING:** **`sed` no longer rejects BRE idioms with a hint** — `\|`,
  `\(…\)`, and `\{N,M\}` used to be a loud `E006` error; they now behave as
  alternation/groups/intervals (see Added). Scripts that relied on the error, or
  on `-E`-free `\|` matching a literal pipe, must use `-E`/`-r` or a bracket
  class.
- **Enumeration guidance points at `keys`/`values`** — the `for x in $VAR` error
  (E012) now leads with `for x in $(values $coll)` / `for k in $(keys $coll)` as
  the way to iterate a collection, and the help gains a Collections syntax section
  plus a stronger onboarding line, so agents reach for the `$()` idiom instead of
  a bare-variable for-head (which stays an error — no implicit word splitting).
- **Agent onboarding composes in importance order** — the always-on instruction
  block an embedder ships (`Recipe::agent_onboarding`) now renders its Foundations
  fragments by an explicit importance rank (most-critical rules first), so the
  client model meets the load-bearing rules even when it skims or truncates. A
  size-budget test keeps the always-on spine lean.
- **BREAKING:** `kaish-help`'s `Fragment` gains a public `rank: u8` field (`0` =
  most important; `UNRANKED` default preserves registry order) plus a
  `Fragment::ranked()` builder. Constructing a `Fragment` literal directly now
  requires the field (a compile error, not silent) — the `en()`/`syntax_section()`
  builders set it for you, so registry authors are unaffected.
- **`help limits` no longer claims `[]` array syntax will never exist** — the
  brackets are reserved by kaish (`[[ ]]` today, native list literals per the
  arrays-and-hashes design). The `[` command stays banned permanently; a `test`
  builtin may return someday.
- **The `collections` help fragment now teaches native literal construction and
  `...` spread** — it previously only showed `fromjson`-based construction,
  stale since list/record literals landed. Membership (`in`/`not in`) is now
  always shown inside a full `if [[ … ]]; then … fi`, never a bare standalone
  test line (a prior regression against the shell's own teaching-note rule).
- **`help collections` documents the one remaining 0.11.0 known limitation** —
  `push` only accepts a top-level bareword target; a bracket-path target
  (`push services[web][tags] item`) fails loudly instead of appending, with the
  read/push/assign-back workaround shown inline. Two other suspected gaps
  (deeply-nested glued list literals, nested `${#path}`/`${path:-default}`)
  turned out already fixed on re-verification and needed no documentation.

### Added
- **`help regex`** — a compact syntax section (also in `help syntax`/`syntax.md`)
  covering the regex dialect in one screen: ERE everywhere (grep/sed/awk), the
  accepted GNU BRE spellings, the bracket-class/`-E` escape hatches for literal
  metas, and the no-backreferences-in-patterns rule.
- **`help collections`** — a new help topic, composed straight from the
  `collections` syntax reference fragment (single-sourced with `help syntax`,
  not a second hand-written doc). The underlying mechanism is generic: any
  `syntax_section` becomes queryable as `help <key>` for free.
- **Native collection literals** — `xs=[a b c]` (list), `{port: 8080}` /
  `{port:8080}` (record, colon may be spaced or unspaced), `xs=[]` / `xs=[dog]`
  (empty and single-element lists), multi-line records with a trailing comma,
  nesting (`{tags: [a b], meta: {active: true}}`), and spread
  (`new=[...$xs date]`, `[...$a ...$b]` — flattens a list operand's elements; a
  bare `$var` inside `[ ]` nests as ONE element instead). Literals are
  value-position only (assignment RHS, the `in`/`not in` RHS operand, and
  nested literal interiors) — never argv (`ls [dog]` stays a glob) or a
  `for`-head item (`for x in [a]` stays a word list). A `[`-leading glob at
  value position (`logs=[0-9]*.log`) is now a loud parse error instead of
  silently reinterpreting the glob; a multi-word bareword record value
  (`{msg: hello world}`) is a loud parse error too — quote it.
- **Native collection read access** — `${xs[0]}`, `${r[key]}`, `${r[$k]}`,
  `${r["weird-key"]}`, `${xs[-1]}` (negative index), `${xs[0:2]}` (end-exclusive
  slice → a list), and chained `${a[b][c]}`, over any `Value::Json` (e.g. from
  `fromjson` or `$()`). Brackets only — a bareword subscript is a literal key, a
  `$var` is dynamic, integers index lists; a subscript that lands on a JSON
  scalar unwraps to a native value (so `[[ ${cfg[healthy]} == false ]]` and
  `$(( ${cfg[port]} + 1 ))` are typed). `${#…}` now returns element count for a
  list and key count for a record (string length unchanged). Every bad access is
  a loud error, never a silent empty: dotted access (`${r.key}` → use `[key]`),
  out-of-bounds index, missing record key, a string key on a list, an integer
  index on a record, and subscripting a scalar. Literal construction
  (`xs=[a b c]`) and record iteration are not in this slice.
- **Collection membership — `[[ e in $coll ]]` / `[[ e not in $coll ]]`** — a
  list tests element membership with typed equality (`[[ 443 in ${servers[web]}
  ]]` matches the JSON number 443, not just the string "443"); a record tests
  key membership. Composes with `&&`/`||`/`!` and works inside a `for` loop like
  any other `[[ ]]` test. A scalar/string RHS is a loud error (never silently
  false/true) — use `=~`, glob (`[[ $s == *sub* ]]`), or `case` for substring
  tests instead.
- **`fromjson` / `tojson` builtins** — the JSON ingress/egress bridge for the
  value model. `fromjson` parses exactly one JSON document (from an argument or
  stdin) into a structured value in `.data`; empty input or trailing garbage is
  a loud line:column error, never a silent `null`, and an object shaped like the
  internal base64 byte-envelope stays a plain record (never silently decoded to
  bytes). `tojson` serializes one value to JSON text (compact, or `--pretty`) —
  the "serialize explicitly first" escape hatch for `export CFG_JSON=$(tojson
  $cfg)`; binary values are refused loudly. Both are pure data (present in every
  capability build). `fromjson "$(tojson $x)"` round-trips `$x` structurally.
- **`keys` / `values` builtins** — the native-collections OPS pair, jq
  semantics over both records and lists: for a **record**, `keys` returns its
  keys and `values` its values (insertion order, pairwise-aligned); for a
  **list**, `keys` returns its indices as integers (`0..N-1`) and `values` its
  elements. This makes `$(keys $c)` / `$(values $c)` the uniform iteration idiom
  over any collection (`for i in $(keys $xs)`, `for x in $(values $xs)`, `for k
  in $(keys $r)`). Both accept a collection directly (no nesting), land the
  result in `.data` for `$()` capture/iteration, and are pure data (present in
  every capability build). A non-collection argument (scalar, bytes, unset) is a
  loud error naming the actual type.
- **Shape guard — `typeof` builtin + `[[ -list ]]` / `[[ -record ]]` test
  operators** — the antidote to the "API sometimes returns an object instead
  of a list" trap. `typeof $x` returns the exact type name (`list`, `record`,
  `string`, `number`, `bool`, `null`, or `bytes`; no int/float split) in text
  output and `.data`, so `t=$(typeof $x)` and `case $(typeof $x) in ...` both
  work. `[[ -list $x ]]` / `[[ -record $x ]]` test the operand's *value*
  shape directly (like `-z`/`-n`, not a path stat like `-f`/`-d`) — the common
  guard idiom is `if [[ -record $data ]]; then ... elif [[ -list $data ]];
  then ... fi` before committing to `keys`/`values`/a `for` loop. A
  defined-but-wrong-shaped value is `false`; a bare unset `$var` is an
  undefined-variable error (like `-z`/`-n`), so a typo isn't silently false.
  `typeof` is pure data, present in every capability build.
- **Path-aware `${#…}` length and `${…:-default}`** — both now accept a
  subscripted path: `${#u[tags]}` is the element/key count of the nested value,
  and `${cfg[port]:-8080}` defaults on the resolved path. The default follows
  decision A — it fires on **absence** (unset root, missing key, out-of-bounds
  index) and on an empty value (JSON `null` or empty string), but never on a
  falsy-but-present value (`false`, `0`, `[]`, `{}`); a **shape** error (integer
  index on a record, string key on a list, subscripting a scalar, dotted access)
  stays loud even with `:-`. These were a loud "bind it first" placeholder
  before.
- **Bare subscripts in arithmetic — `$(( xs[i] ))`** — inside `$(( … ))` a
  bracket's contents are a numeric expression, so a bareword subscript is the
  **variable** `i` (the opposite of the interpolation form `${xs[i]}`, a literal
  key). `xs[0]`, `xs[i]`, `xs[i + 1]`, `xs[-1]`, and chained `grid[i][j]` all
  work; an out-of-bounds index is loud. Previously a bare subscript was dropped
  and failed as "variable is JSON, not a number".
- **Collection lvalue writes + `push`** — bracket paths are now assignment
  targets too: `xs[0]=9` (in-bounds list index update, negative indices work),
  `user[email]=x` (record key insert-or-update), and deep paths
  (`services[web][port]=9000`). No autovivification — every intermediate
  segment must already exist with the right shape; the ONLY thing a path-set
  may create is the final record key. An out-of-bounds index set, a missing
  intermediate, a scalar/undefined root, and a slice lvalue (`xs[0:2]=x`) are
  all loud errors, never silent. `push NAME VALUE...` appends to a top-level
  list variable in place (bareword target, like `read`/`unset`); the target
  must already exist and be a list. The validator now rejects a dotted
  assignment target (`user.email=x`, since the `Ident` token admits `.` for
  other uses) with a bracket-form suggestion, and a subscripted assignment
  whose root isn't already bound. Env-prefix assignment (`X=v cmd`) stays
  bare-ident-only — a subscripted target there is never captured as an
  exported command-scoped variable.
- **`json_to_value_no_envelope` (kaish-types)** — envelope-free JSON→`Value`
  conversion for external JSON, so byte-envelope-shaped objects are never
  silently decoded to `Value::Bytes`.
- **`ExecResult::latch_request()` + `LatchRequest` (kaish-types)** — typed
  embedder seam for the confirmation latch: decodes a latched result (exit 2 +
  nonce payload) into `{nonce, command, paths, hint, ttl}` so an embedder can
  apply preapproval policy or a model review before re-running with
  `--confirm=<nonce>`, instead of string-matching the error or digging `.data`
  by key. Returns `None` for plain (non-latch) exit-2 results.
- **`Kernel::classify_command(name) -> CommandKind` (kaish-types)** — supported,
  single-source-of-truth command classifier for embedders preflighting a script
  (e.g. a consent gate that walks the public AST and blocks until external
  commands are approved). Buckets a name into `Builtin` / `UserTool` / `Special`
  / `Dynamic` / `External`, mirroring the interpreter's real resolution order so
  the embedder never forks kaish's rules. `CommandKind::escapes_kernel()` flags
  the `External`/`Dynamic` cases a gate must scrutinize.
- **`grep`, `sed`, and `awk` accept the GNU BRE backslash-metas** (`\|`, `\+`,
  `\?`, `\(`, `\)`, `\{`, `\}`) as a superset of their ERE regex, so
  agent-idiomatic `grep 'foo\|bar'`, `sed 's/cat\|dog/X/'`, and
  `awk '/a\|b/'` now alternate (and `sed 's/\(a\)\(b\)/\2\1/'` groups) instead
  of silently matching a literal `|`/`(` and returning nothing (issue #60).
  Pass `-E` (`grep`) or `-E`/`-r` (`sed`) for strict ERE, where those escapes
  match the literal character — the escape hatch for a literal `|`/`+`; `awk`
  has no such flag and always rewrites. Bare ERE (`foo|bar`) and `-F` are
  unchanged. Narrow trade-off: in the default dialect a backslash before one of
  these metas is the operator, never a literal — match the character itself with
  a bracket class (`[+]`, `[|]`). A stray `\)`/`\{` that no longer parses fails
  loudly, with a hint naming the dialect, the bracket-class spelling, and
  (grep/sed) the strict-ERE flag. awk field splitting stays gawk-compatible:
  `-F '\|'`, `FS="\\|"`, and `split(s, a, "\\|")` split on a literal pipe (the
  rewrite runs before the single-char-FS-is-literal rule, matching gawk's
  demotion of `\|` to plain `|`).

### Removed
- Validator advisory **W006** (`PosixTestCommand`, which steered `test`/`[`
  toward `[[ ]]`) is retired — `test` is a first-class builtin now and validates
  through the registry like any other command.

### Fixed
- **`[[ ]]` file tests honor `cd`** (GH #101). A relative path in `[[ -f x ]]`
  (and `-e -d -r -w -x`) used to stat against the process cwd, so after a `cd` an
  existing file read as absent — a silent wrong `false` into `if`/`&&`. It now
  resolves against the session cwd, agreeing with the VFS-aware `test` builtin.
- **Binary data at text sinks is loud, not a silent placeholder** — a
  `Value::Bytes` value (e.g. `b=$(cat some-binary-file)`, since `cat` emits raw
  bytes for non-UTF-8 content) used to render the placeholder `[binary: N bytes]`
  when interpolated into a string (`"x=$b"`), passed as a bare word to an external
  command (`prog $b`), or printed by `echo`/`printf`. These text sinks now error (`binary
  data (N bytes) cannot be used as text — decode it (base64/xxd) or redirect to a
  file`); valid-UTF-8 bytes still coerce. Semantic uses (`==`, `in`, `${#…}`,
  `case`) are unchanged. Path-positional/env/redirect sinks remain a known gap.
- **Binary worker output no longer corrupts `gather`** — scatter/gather result
  rows and `gather --lines` lossily UTF-8-decoded (`U+FFFD`) a worker whose stdout
  was binary; a row now reports `ok:false` with a clear error and `--lines`
  hard-errors (exit 123) instead of emitting mangled bytes.
- **`scatter`/`gather` reject wrong-typed flag values loudly** — `--limit`,
  `--as`, and `--timeout` silently fell back to their defaults when handed a
  value of the wrong type (a string `--limit "5"` from a variable, a negative
  `--timeout`) or a `$(...)` the reduced sync binder can't evaluate; they now
  coerce where sensible (`--limit "5"` → 5) and otherwise fail with a usage error
  instead of running mis-configured.
- **`scatter` workers inherit the parent execution context** — workers ran with a
  from-scratch context that dropped the script watchdog (so a `ctx.patient` hold
  suspended a missing clock → false-positive request-timeout kills) and bypassed
  the output-limit memory cap (N parallel workers could each hold unbounded
  output). Workers now derive from the parent, carrying the watchdog, output limit
  (with a per-worker spill boundary), aliases, and ignore/external-command policy.
- **Backend-tool results keep their structured data across the kernel seam** — a
  backend-registered tool's `ToolResult` → `ExecResult` conversion carried only
  `.out`, dropping `.data`/`content_type`/`baggage` (so `x=$(embedder_tool)` lost
  a structured/collection result and OTel baggage). A symmetric conversion now
  preserves them, and a real backend execution error is reported as a failure
  rather than masked as exit-127 "command not found".
- **`source` / PATH `.kai` scripts accumulate output across statements** — they
  used to keep only the last statement's stdout, silently discarding everything
  earlier.
- **`Kernel::reset()` preserves embedder state** — it silently reverted the
  confirmation-latch/trash flags, the kernel's `$$` identity, and frontend-seeded
  `initial_vars` (HOME/PATH), so an embedder resetting between requests lost its
  latch gate. `reset()` now keeps all three.
- **`jq` no longer JSON-sniffs a `Value::String` `.data` payload** — `fromjson
  '"1"' | jq type` reported `"number"`; a string is now converted directly,
  honouring the no-sniffing invariant.
- **The argv door no longer panics on a non-ASCII short-flag bundle** —
  `execute_argv("ls", ["-lé"])` sliced mid-codepoint and panicked; the classifier
  now only accepts the lexer's real short-flag char class, falling back to a
  literal positional.
- **A bareword `key=value` positional after `--` parses** — `echo -- a=b` used to
  hard-fail.
- **A stdout redirect now clears the structured `.data` sideband too**, not just
  `.out`/`.output`. `.data` is the structured view of stdout, so `> file` /
  `>> file` / `&> file` take it along with the text: `x=$(cmd > file)` captures
  `""` (bash parity) and `cmd > file | consumer` no longer leaks a structured
  value past its own redirect. A pending confirmation-latch request is the one
  exception — it is a control-plane signal, not stdout, so it survives a
  redirect (a latched `rm precious > log` still gates). Consequence: recovering
  a redirected command's `.data` after the fact (`cmd > /dev/null; kaish-last`)
  no longer works — don't redirect the output you meant to capture.
- **`"$(cmd)"` no longer drops a `.data`-only result to `""`** — quoted command
  substitution used to read only a command's `.out` text; a tool that set
  structured `.data` (a list/record) but left `.out` empty interpolated to
  nothing, silently losing the collection. It now falls back to rendering
  `.data` the same way a bare `"$x"` renders a collection-valued variable
  (compact JSON for lists/records, plain form for scalars); non-empty `.out`
  still wins unchanged.
- **`gather`'s own trailing redirect (`… | gather > file`) is no longer silently
  dropped.** Found while wiring up the JSONL doors (GH #80): the scatter/gather
  special-cased pipeline path never applied `gather`'s redirects when it was
  the pipeline's last command — the JSONL rows landed back in the result
  instead of the file.
- **`Kernel::with_backend` now mounts `/dev` (DevFs) unconditionally** —
  custom-backend embedders (e.g. kaijutsu) previously had no `/dev/null`,
  `/dev/zero`, `/dev/random`, or `/dev/urandom` at all, and `VirtualOverlayBackend`
  hardcoded routing to the `/v/*` namespace only, so even an embedder that
  mounted its own `/dev` would have the mount silently ignored. The overlay now
  also routes any path covered by a real VFS mount (not just `/v/*`), and
  `with_backend` mounts `/dev` by default — a read-only embedder backend (like
  kaijutsu's read-only host root) no longer turns `cmd > /dev/null` into a
  filesystem error.
- **`${#path}` on an undefined subscripted root is now loud** — `${#nope[items]}`
  silently returned `0` (the bash-parity `${#unset}` → 0 rule leaked into
  subscripted paths), so a typo'd name in a length-guarded loop spun zero times
  with no diagnostic. Bare `${#unset}` stays `0`.
- **Double-quoted record-literal keys now interpolate** — `{"$k": v}` resolves
  `$k` like any double-quoted string instead of silently creating a literal
  `"$k"` key. Single quotes keep a literal `$` (`{'$k': v}`), same quoting
  contract as everywhere else.
- **A standalone `[[ ]]` test now writes `$?`** — `[[ 1 = 2 ]]; echo $?` printed
  `0` (the previous command's status; the test never stored its result), so
  `[[ -f x ]]; ok=$?` patterns silently read a stale status. A bare test
  statement now sets `$?` and honors `set -e` like any command, matching bash;
  `&&`/`||` operands and `if`/`while` conditions are unaffected.
- **awk invalid FS/`split()` separator errors name the separator as written**
  (not the internal ERE-rewritten form) and carry the GNU-BRE dialect hint when
  the rewrite is the likely culprit.
- **Collection read-access silent-corruption traps** (found by the collections
  milestone review) — five cases that returned a plausible wrong answer instead of
  erroring, all now loud:
  - Comparing a list/record to a scalar with `==`/`!=` (e.g. `[[ $list == x ]]`)
    was silently `false`; it is now a loud error hinting at `in`/`jq` (a JSON
    scalar still unwraps and compares typed).
  - `export`ing a structured value to a subprocess silently JSON-serialized it
    into the child's environment; it is now refused, pointing at `export
    CFG=$(tojson $CFG)`.
  - A `fromjson` array element shaped like the internal byte-envelope was silently
    decoded to binary during `for`/`jq` iteration; it now stays a record.
  - `${#…}` in the scatter/gather sync path returned the string byte-length
    instead of the element/key count (`${#xs}` on a 3-element list gave `7`);
    `${#…}` on binary now counts bytes.
  - A negative slice *end* (`${xs[0:-1]}`) was misread as a `:-default` and
    mangled to `1]`; the `:-` scanners are now subscript-aware. (Length and
    default on a *subscripted* path — `${#u[tags]}`, `${cfg[port]:-8080}` — were
    briefly a loud "bind it first" error; they are now fully path-aware, see
    Added.)
- **`scatter`/`gather`'s own flag values (`--as`, `--limit`, …) now fail loud on
  a bad/subscripted collection access** — the reduced sync arg binder they use
  (it runs once, before workers fork, so it can't recurse through the async
  pipeline) used to discard a missing-key/shape `PathError` as a silently
  dropped flag or an omitted `${#…}` length instead of erroring, unlike `echo`,
  assignment, `$(( ))`, and `"${…}"`. A bad `scatter --as ${u[nope]}` now fails
  the whole pipeline instead of quietly falling back to a bare boolean flag; a
  subscripted path on an entirely undefined root (`${nope[key]}`) is loud too,
  while a bare undefined `$VAR` keeps the bash-compatible coalesce.
- **A bare collection reaching an external command's argv or a redirect
  target silently JSON-serialized** (e.g. `curl -d $cfg` or `echo x > $cfg`);
  every process-boundary sink now refuses it with a `$(tojson $x)` hint. This
  covers the generic external-argv/redirect paths **and** the three subprocess
  builtins that build argv/env themselves (`spawn`, `exec`, `env`) — including
  `spawn`'s nested-collection argv elements and `env <cmd>`'s child-environment
  export (which previously bypassed the OS-env-export guard). A quoted `"$cfg"`
  is unaffected — string interpolation renders compact JSON before either sink
  sees it.
- **Scalar-only `[[ ]]` test operators silently stringified a collection
  operand** — `-z`/`-n`, `=~`/`!~`, ordering (`<`/`>`/`<=`/`>=`), and numeric
  (`-eq`/`-ne`/`-gt`/`-lt`/`-ge`/`-le`) now refuse a list/record operand with a
  loud Shape error naming the operator (e.g. an empty list no longer reads as
  `-z`-true via its JSON text `"[]"`). `==`/`!=` already errored; `in`/`not in`
  are unaffected (the one operator family that legitimately takes a
  collection).
- **`${x:-default}` no longer treats JSON `null` as present** — a variable
  holding `null` stringified to the non-empty `"null"`, so `${x:-fallback}`
  returned `null` instead of the default. `:-` now fires on `null` and empty
  string (decision A), while `false`/`0`/`[]`/`{}` stay present values.
- **`--help`/`-h` now passes through to `with_owned_output()` tools** — the
  kernel's generic help router no longer intercepts it for tools that re-parse
  their own argv, so a leaf request like `tool subcmd --help` reaches the tool
  and renders its subcommand help instead of the top-level whole-tool help. This
  also retires the per-tool workaround of advertising a synthetic root `help`
  param. Plain tools are unaffected.

## [0.10.0] - 2026-06-29

### Added
- **`grep --ftype <NAME>` / `--ftype-not <NAME>`** — filter the recursive (`-r`)
  walk to (or away from) a named file type, e.g. `grep -r needle . --ftype rust`
  (searches `*.rs`). Both are repeatable. `--ftype` is the kaish-wide file-type
  filter (distinct from file/dir *entry-kind*). An unknown type name is a loud
  exit-2 error, never a silent empty match.
- **`grep --ftype-list`** — print the known file types and their globs as a
  TYPE→globs table (works with `--json`); no pattern required.
- **`grep --hidden`** — include dotfiles/dot-directories in the recursive walk
  (off by default, bash no-dotglob semantics).
- **`grep --max-count <N>`** — stop after N matching lines per file (GNU
  semantics; `--max-count 0` matches nothing). Streaming single-file and piped
  searches stop reading once the cap is hit, so it bounds work on large inputs.
- **`glob --ftype <NAME>` / `--ftype-not <NAME>` / `--ftype-list`** — the same
  file-type filtering on `glob`, sharing grep's type registry. `--ftype` (rg-style
  file type, by extension) is distinct from glob's existing `-t`/`--type` (entry
  *kind*: file/dir); the two compose. A file-type filter narrows files only —
  directories pass it untouched. Unknown type → loud exit 2.
- **`Kernel::execute_argv(name, &[Value])`** (embedder API) — argv-native peer of
  `execute(&str)` for callers that already hold tokenized arguments. Runs one
  command with **literal** tokens (no glob/`$VAR`/command-substitution/splitting)
  and passes typed `Value`s (`Bytes`/`Json`) straight through as positionals
  without the lossy `to_argv()` stringification a re-quoted string would force. It
  reuses the full dispatch chain (`--json`, the confirmation latch, command
  resolution), so behavior matches the string door for single commands.
- **`file` builtin identifies files by content (magic bytes), not extension.**
  Reports a broad type word plus MIME (`photo: image (image/png)`); `-i`/`--mime`
  prints the MIME alone, `-b`/`--brief` drops the filename, `--json` emits a
  `FILE`/`TYPE`/`MIME` table. Reads a bounded prefix through the VFS (works in
  sandboxed/overlay backends) and classifies stdin when given no paths.
  Detection is magic-first with a UTF-8 text fallback (`text/plain`); zero bytes
  report `empty`, and otherwise-opaque bytes (headerless raw PCM, non-UTF-8
  binary) report `data` — never a guess. Detection lives in `kaish-glob`'s new
  `filetype` module
  (pure-Rust `infer`, no C deps) — `detect` for magic-only, `classify` for the
  text-aware path — so embedders can classify bytes the same way the shell does.
- **`sed -i` edits files in place** instead of streaming to stdout, across one or
  more file operands. It is always a truncating overwrite, so it routes through the
  same latch/trash gate as `tee`/`patch` below. With no file operands it's a loud
  error (editing a stream in place is meaningless). Each file is written with a
  whole-file compare-and-swap (like `patch`), so a concurrent change between read and
  write is a loud conflict, never a silent clobber; a per-file failure doesn't abort
  the batch and every error is reported. The GNU glued backup suffix (`-i.bak`) is
  **not yet supported** — kaish's lexer splits `-i.bak` at the dot; the trash snapshot
  under `set -o trash` already keeps a recoverable prior copy.
- **The write-model gate (`set -o latch` / `set -o trash`) covers every truncating
  overwrite** — `tee`, `patch`, `sed -i`, **and now `write`, `cp`, `mv`, and `dd of=`**
  (these four previously bypassed it) — like `rm` gates a delete. Overwriting an
  existing file under `trash` first copies its prior content to trash (recoverable,
  via the new `TrashBackend::trash_bytes`), leaving the file in place for the new
  content; under `latch` it needs `--confirm=<nonce>` (exit 2 until confirmed, one
  nonce scoping all files a command touches; `dd` uses its own `confirm=<nonce>`
  key=value idiom). A new file, an append (`tee -a`), `patch --dry-run`, or a copy/move
  *into* a directory never gates. The byte-oriented writers (`tee`/`write`/`dd`/`cp`)
  now compare-and-swap against the gate's snapshot, so a concurrent change between the
  gate and the write is a loud conflict rather than a silent clobber (`mv`'s same-mount
  rename is already atomic). A prior file too large to snapshot (over the trash
  max-size) falls through to `latch`/proceed, matching `rm`. Both gates are off by
  default, so default behavior is unchanged.
- **Validator advisory `W006` steers `test` to `[[ … ]]`.** Using a bare `test` as a
  command now surfaces a one-time stderr note (`use [[ … ]] …`) and still runs. A
  path-qualified form (`/usr/bin/test`, `./test`) is left alone — it's an explicit
  external-binary call, not the conditional footgun.
  This is the first *agent-surfaced* validation warning: most warnings stay
  trace-only (every external command fires `UndefinedCommand`), but a code can
  opt into surfacing via `IssueCode::surfaces_to_agent` — the seam for a broader
  "did-you-mean" guidance pass. The advisory matters because a bare `test` would
  otherwise resolve to an external `/usr/bin/test` that evaluates against the real
  host filesystem, bypassing the VFS/overlay (a silent wrong boolean into `if`/`&&`).

### Changed
- **BREAKING: `cp` no longer accepts `-p`/`--preserve`.** It was parsed and
  silently discarded (the VFS has no mode, mtime, or ownership to preserve), so
  `cp -p src dst` used to copy and exit 0; it now exits 2 as an unknown-argument
  usage error. Drop the flag — plain `cp` performs the same copy.
- **`jq` renders integral results without a trailing `.0`** — `6/2` → `3`,
  `1e10` → `10000000000` (were `3.0` / `10000000000.0`), and large integers print
  exactly on stdout. (jaq-core 3 / jaq-json 2.)
- **`jq` object keys now preserve insertion order** instead of being sorted
  alphabetically — `echo '{"b":1,"a":2}' | jq .` emits `{"b":1,"a":2}`, matching
  real jq. This comes from upgrading the native jq engine to jaq-core 3 / jaq-json 2
  and enabling `serde_json`'s `preserve_order` workspace-wide, so any tool emitting
  JSON objects (`--json` output, structured `.data`) now keeps source/insertion
  order rather than sorting keys. `jq keys` (sorted) is unchanged; `keys_unsorted`
  now correctly reports source order.

### Removed
- **BREAKING: the unused `Executor` trait and `NoOpExecutor` are removed from the
  public `interpreter` surface.** The sync expression evaluator no longer takes an
  executor — command substitution is resolved by the async evaluator, which
  pre-resolves operands to literals before any sync evaluation, so the sync path's
  command-substitution/file-test arms were dead code. Pre-1.0, breaking-in-name
  only; nothing implemented or consumed them.
- **BREAKING: the `test` builtin and `[` command are gone.** Use `[[ … ]]`, the one
  supported test form. The removed builtins never worked end-to-end through the
  parser anyway: `[` could not be a command name and `test`'s comparison operators
  (`test a = b`, `-eq`, `<`, `>`) were rejected in argument position — only unary
  `test -z`/`test -f` routed. `[[ … ]]` is preferred because the validator
  understands it and can catch a malformed test before runtime, where `test`/`[`
  hide their operators as runtime string arguments.

### Fixed
- **Listing a live directory no longer fails when one entry vanishes mid-scan.**
  `LocalFs::list` (`ls`, and any walk over a real directory) stats each entry in a
  step separate from the `read_dir` that yielded it, so a sibling unlinked in that
  window (a concurrent writer, a build churning `target/`, an editor's temp file)
  made the whole listing fail with `ENOENT` (`ls: .: not found`). A vanished entry
  is now skipped, the way `ls(1)` tolerates a file removed mid-scan; a *dangling*
  symlink still lists (its link stat succeeds), and other stat errors still surface.
- **`--json` no longer drops the structured payload of an error result.** A
  non-zero exit that carries both a diagnostic message and structured `.data`
  — notably the latch confirmation nonce from `rm`/`tee`/`patch`/`sed -i` — kept
  only `{error, code}` under `--json`, silently clobbering the nonce. The error
  envelope now nests the payload under `data`: `{error, code, data: {nonce, …}}`.
- **`sed -e <number>` is a loud error, not a silent drop.** A non-string `-e`
  expression (e.g. `sed -e 5`) now exits 2 with a usage error instead of being
  ignored — silently dropping it once let the *filename* be parsed as the program.
- **Unterminated arithmetic `$(( …` is a lexer error**, not a silent partial
  evaluation — `$(( 1 + 2` (no closing `))`) no longer evaluates to `3`.
- **`sed 's/x/y/0'` is a loud error**, matching GNU — the `0` occurrence flag
  ("replace the 0th match") was silently treated as the first match.
- **`printf '%c'` honors width and the left-align flag** (`printf '%5c' x` →
  `␠␠␠␠x`), and a `\c` in a `%b` argument — or in the format literal — now stops
  *all* output rather than only the rest of that argument, matching GNU `printf`.
- **File tests expand `~`.** `[[ -f ~/x ]]` resolves the tilde against the session
  `HOME` before stat'ing (it was always false). Hermetic kernels with no `HOME`
  keep `~` literal.
- **`<<-` strips leading tabs from the literal source only**, no longer eating
  tabs that came from an interpolated variable's value (bash strips source-line
  tabs before expansion).
- **`export NAME=VALUE` inside a function persists after the function returns**,
  matching a plain assignment and bash — it was previously dropped with the
  function frame.
- **`jq` no longer errors on indexing `null`** — `null | .a` yields `null` (jq
  parity), from the jaq-core 3 / jaq-json 2 upgrade.
- **`write` no longer corrupts binary content from `$(…)`.** `write FILE $(producer)`
  (and `producer | write FILE`) now persists the raw bytes of a `Value::Bytes`
  verbatim instead of stringifying it to the `[binary: N bytes]` placeholder — that
  placeholder reaching a file was silent corruption. Content from a positional,
  `--content`, or stdin is all read as bytes. (`tee` already read stdin as raw bytes
  and is unaffected; pinned by a regression test.)

## [0.9.1] - 2026-06-25

### Added
- **BREAKING: `diff --json` emits structured hunks** instead of the unified-diff text wrapped as a JSON string: `{old_file, new_file, differ:true, hunks:[{old_start, old_lines, new_start, new_lines, changes:[{tag, content, missing_newline}]}]}` (`tag` ∈ `equal`/`delete`/`insert`, `content` newline-stripped for clean `jq`, `missing_newline` flags a line with no terminal newline so a final-newline-only change stays distinguishable). Plain `diff` output is unchanged; `diff -q --json` reports `{old_file, new_file, differ:true}`. Identical files emit a consistent `{old_file, new_file, differ:false, hunks:[]}` object (exit 0, empty text) so a `--json` consumer always parses an object, never an empty string.

### Changed
- **BREAKING: `patch` now applies with offset search and fuzz, like GNU `patch`** — instead of hard-failing on any line drift. A hunk is located by matching its context near the header position (reporting `Hunk #N succeeded at L (offset O lines)` when it lands elsewhere) and, if exact context fails, by ignoring up to 2 lines of context at each end (`with fuzz F`, searched within a bounded window of the header position). Offsets/fuzz are reported loudly; a hunk that matches nowhere still fails loud (`Hunk #N FAILED`) and the file is left untouched (whole-file compare-and-swap write). A clean apply stays quiet. CRLF line endings are preserved. **Exit-code contract change:** a near-miss patch that previously exited non-zero (drift rejected) now applies and exits 0 — scripts that branched on `patch`'s exit to detect drift should read the `offset`/`fuzz` report instead. This relaxes kaish's previously stricter-than-GNU behavior so agents' near-miss diffs apply instead of looping; content-anchored editing remains an embedder concern (see `docs/editing-for-agents.md`).

### Security
- **Hermeticity: external-command resolution no longer falls back to the OS `PATH`.** `try_execute_external` (and the test-only `BackendDispatcher` spawn site) read `PATH` from kernel scope only — when it's absent they no longer reach into `std::env::var("PATH")`, which contradicted the "kernel never reads OS env" contract. A frontend that wants host `PATH` seeds it via `initial_vars` (the REPL already does, with `os_env_vars()`); an embedder that doesn't gets a kernel that can't resolve external commands at all (the strongest hermetic default). No behavior change for the REPL or any embedder that seeds env.

### Fixed
- **`grep -c` exits 1 when nothing matched** (GNU parity), instead of always exiting 0. The count is still printed; over multiple files it exits 1 only when no file matched.
- **`$(…)` command substitution strips only trailing newlines**, not all trailing whitespace — significant trailing spaces/tabs in the captured output now survive (`x=$(printf 'a  ')` keeps the spaces), matching the `"$(…)"` interpolation and for-loop paths. Previously a bare `$()` used `.trim_end()`.
- **`jq '. / 0'` fails loudly** instead of silently returning `null`. jaq evaluates `n/0` to a non-finite float (`inf`/`NaN`) that JSON can't represent, which was being coerced to `null`; any non-finite numeric result in jq output is now a loud `jq runtime error` (exit 1). (Modulo by zero already errored.)
- **Common-idiom words now lex as one bareword** instead of fragmenting into adjacent tokens and tripping the no-token-pasting guard (a loud-but-wrong parse error). Covered: bare `@` (`user@host`, `a@b.com`, `@scope/pkg`, the `date -d @0` epoch form); digit-leading hyphenated words — ISO dates (`2024-01-02`, leading zeros preserved), `N-M` ranges (`10-20`, `cut -f 1-3`), and float-dash forms (`1.5-2`); and minus-led numeric predicate values (`find -size -1k`, `-30d`). A plain `2024`/`1.5`/`-1` still lexes as `Int`/`Float`. **Behavior shift:** `tr -d 0-9` (an unquoted digit range) was previously made a loud error as a stopgap; it now reaches `tr` verbatim and deletes digits, matching bash/GNU. The no-pasting guard still fires on genuinely separate adjacent words like `echo 1,2,3`.
- **`glob` errors on zero matches** (exit 1 with a message naming the pattern) instead of silently exiting 0 — honoring kaish's documented strict-glob guarantee.
- **`env -u NAME` / `env -i` apply when listing** (no command operand), not only when running a command — `env -u SECRET` no longer prints `SECRET`. (`-u` is repeatable.)
- **`readlink -f` and `realpath` resolve symlinks** through the VFS (following chains, with a loop guard) instead of only normalizing `.`/`..`; `realpath` errors on a nonexistent path, and bare `readlink` on a non-symlink reports "not a symbolic link".
- **`ls -1` is the single-column flag** (was lexed as the integer `-1` and treated as a path operand).
- **`basename //` and `dirname //` return `/`** (POSIX), not `//` and `.`.
- **`tail -c +N` counts from byte N** (from the start) instead of treating `+N` like the last N bytes; `tail -c N` is unchanged.
- **`tree <nonexistent>` errors** (exit 1) instead of printing a bare root node with exit 0.
- **`diff --json` emits a consistent object shape** across all three paths — `old_file`, `new_file`, `differ` are always present; `hunks` is present whenever not in `-q` mode (the full-diff path was missing `differ`).
- **`dd skip=N` skips input even without `count=`** (the offset was only applied when a count was given).
- **`seq -w` pads negative numbers** to equal width (the sign is counted), instead of widening only the magnitude.
- **`scatter` treats a single JSON object/scalar `.data` as one item** instead of newline-splitting its pretty-printed text into bogus items; arrays still fan out per element.
- **A syntax error inside a quoted `"$(…)"` is now a loud parse error**, matching the unquoted `$(…)` form, instead of being silently demoted to literal text — `echo "$(if true; echo 1; fi)"` (malformed) errors at parse time rather than printing the literal string. (A malformed `$()` inside a `${VAR:-default}` *default word* still falls back to literal — a rare edge.)
- **`&&`/`||` now have equal precedence and associate left-to-right** (POSIX), instead of `&&` binding tighter than `||`. `true || echo A && echo B` prints `B` (was: nothing); the `[[ ]]` compound path was already correct.
- **Structured data now survives builtin→builtin pipelines** (`seq 1 3 | jq .`, `… | cut -f1 | jq length`). The consumer used to `try_recv` the producer's typed `.data` once at startup and — losing that race on a multi-thread runtime — fall back to parsing the pipe *text*, so `jq` choked with "invalid JSON … trailing characters". The consumer (`jq`, `scatter`) now resolves stdin by draining the pipe first (which can't deadlock a streaming upstream) and *then* awaiting the structured-data sideband, which is deterministic regardless of scheduling.
- **`printf` now applies flags, precision, and the missing conversions** instead of silently ignoring them: `+`/space/`#` flags; `.precision` for `%s` (truncate) and `%d`/`%x`/`%o` (min-digits, and precision overrides the `0` flag); and `%u`/`%E`/`%G`/`%b` (previously emitted the literal `%X`) plus the `\0NNN` octal escape. Width, `-`, and basic `%s`/`%d`/`%x`/`%f` were already correct.
- **`tr` interprets C-style escapes in SET1/SET2** (`\n` `\t` `\r` `\\` `\f` `\v` `\a` `\b` and octal `\NNN`) — `tr '\n' ' '` now matches a newline, not the literal letter `n`.
- **`sort -k` honors the key** — glued (`-k2n`) and separated (`-k 2`) field keys, `-k2,2` single-field range, per-key `n`/`r` modifiers, the field-to-EOL default, and `-u` deduping by the key (not the whole line); previously a `-k` spec silently fell back to full-line sort.
- **`cut` dedups and orders selected ranges** — `cut -f 1-3,2-4` no longer doubles the overlap and `cut -f 3,1` emits fields in ascending order (union semantics), for `-f`/`-c`/`-b`.
- **`cat -n` preserves the input's trailing newline** — it was dropping the final `\n` (a lossy `.lines().join`), silently corrupting the last line through a pipe.
- **`find` predicates work**: a regular-file operand prints itself; `-maxdepth 0` restricts to the start path; and `-mindepth`/`-path`/`-ipath` are parsed (they were swallowed as `-h` and printed help with exit 0).
- **`[[ … =~ … ]]` errors loudly on an uncompilable regex** instead of silently evaluating false (which was indistinguishable from a clean non-match). A valid non-match still returns exit 1.
- **Background jobs no longer deadlock under `wait`.** `JobManager::wait` held the `jobs` mutex across the awaited job's completion, so while a `wait %N` was parked every other job operation (a nested `&` spawn, `jobs`, status reads, `kill`) blocked until that job finished — a nested `&` under `wait %N` hung (exit 124). `wait` now polls for completion in short intervals, releasing the `jobs` lock between polls (so every other job op runs freely) and never carrying the job's handle off the map — a waiter dropped mid-wait (`timeout N wait %1`) leaves the result in place for a later `wait %1` instead of orphaning it. Relatedly, `JobManager::spawn` is now `async` and inserts under `lock().await` instead of busy-spinning on `try_lock` (which on a current-thread runtime could livelock the executor). *(Embedder note: `JobManager::spawn` changed from sync to `async` — add `.await`. Niche surface; not in the curated crate-root re-exports.)*
- **External commands no longer deadlock on large buffered stdin.** A buffered `String` stdin (e.g. `ExecuteOptions::with_stdin`, `printf … | kaish -c cmd`) was written to the child *inline*, before the output-drain spawned — so a child that emits a lot before consuming all its input filled both pipe buffers and hung forever (anything past ~a couple pipe-buffers' worth, e.g. a multi-MiB payload through `cat`). The write now runs in a detached task concurrent with the drain, matching the streaming-pipe path. A child that closes stdin early (`head`) is also handled cleanly now (a broken-pipe write is no longer reported as a command failure).
- **Safety: `rm -r <symlink-to-dir>` no longer deletes the link target's contents.** `rm`/`mv` followed a symlink through to its target (path resolution canonicalized the whole path, and the recursive walk `stat`'d *through* the link), so removing a symlink-to-dir descended into and deleted the real target. Both now route through one symlink-safe recursive remover on the backend: a symlink is `lstat`'d (never `stat`'d) and unlinked/renamed in place, never followed. `rm <symlink-to-dir>` (with or without `-r`) unlinks just the link; a symlink child inside a recursively-removed dir is unlinked, not descended; under `set -o trash` a symlink bypasses trash entirely (trashing would resolve through to the target) and is unlinked directly. `mv` of a symlink (incl. a dangling one) now renames/recreates the link rather than moving its target. Consolidation also removed two duplicate hand-rolled recursive removers (`rm`, `mv`) in favor of `backend.remove(path, recursive)`.
- **Validation no longer false-errors on glued value-flags with a file operand**: the pre-execution validator used a schema-blind arg-builder, so `sed -e1d -e2d FILE` (and `awk` equivalents) left `-e` unbound and fell back to parsing the *file path* as the program — a path-dependent false `E006` (e.g. a path containing `t` → "unknown command"). The validator now binds glued/value short-flags the same way execution does, so it sees the real expressions. Relatedly, the glued slash form `sed -es/a/b/` now reports the accurate "s command requires delimiter" instead of a misleading "unknown command".
- **Combined short flags now bind a trailing value flag** (`grep -ivC 3`, `grep -inC1`): the value-taking flag in a bundle used to be treated as a boolean, stranding its argument as a stray positional → arity error. Bool flags still stack (`ls -la`); the first value-taking flag in the bundle consumes the rest of the token (`-ivC3` → `C=3`) or, if it's the last char, the next positional (`-ivC 3` → `C=3`).
- **Glued repeatable short flags accumulate instead of clobbering**: the glued first-char form of a repeatable flag (e.g. `sed -e1d -e2d`) now keeps every value like the separated `-e 1d -e 2d`, instead of silently dropping all but the last.
- **`sed` no longer false-errors valid ERE as a BRE capture-group idiom**: a literal `\\1` in the replacement (escaped backslash + `1`) is no longer mistaken for a `\1` backref; a pattern that mixes a real ERE group `(a)` with literal escaped parens `\(b\)` plus a `\1` backref is accepted; and — conversely — a literal `(` inside a bracket class (`[(]\(x\)`) no longer *suppresses* the BRE error, so a genuine BRE capture-group mistake beside it still fails loud.

## [0.9.0] - 2026-06-18

### Added
- **`sort -V` / `--version-sort`** orders version-like strings naturally — digit runs compare by value, so `v1.2` < `v1.9` < `v1.10` (was an `unexpected argument` error).
- **`kaish -c '…'` (and `kaish script.kai`) now read piped stdin** when invoked non-interactively, so `printf 'data' | kaish -c 'sort'` feeds the top-level builtin instead of silently producing nothing. Stdin is fed **lazily**: a command that reads stdin drains it, but one that doesn't (`echo`) returns immediately even when stdin is an open pipe that never sends EOF — `sleep 10 | kaish -c 'echo hi'` prints `hi` and exits at once rather than hanging. Stdin is consumed by the first command that reads it (shell draining semantics); a redirect (`< file`/heredoc) still takes precedence; binary stdin survives losslessly. Two embedder seams: `ExecuteOptions::with_stdin(String)` for a ready buffer, and `Kernel::execute_with_pipe_stdin(_streaming)` taking a `PipeReader` for a lazy, byte-clean stream (the CLI uses the latter).
- **`sed` ergonomics pass** (gaps chosen from a cross-model usability panel — see `docs/designing-syntax-with-llms.md`): `;` now chains multiple commands in one expression (`sed 's/a/b/; s/c/d/'`); `s///N` / `s///Ng` act on the Nth match; `a TEXT`/`i TEXT`/`c TEXT` append/insert/change lines (all of `a\TEXT`, `a TEXT`, `aTEXT`); `y/abc/xyz/` transliterates; and `-E`/`-r` are accepted as no-ops (kaish sed is always ERE).
- **`awk` range patterns** `/start/,/end/` (and expression endpoints like `NR==2,NR==5`) — fire inclusively from the record matching `start` through the record matching `end`.
- **`awk` fails loud on unsupported constructs** instead of mis-parsing or silently doing nothing: output redirection (`print > f`, `>>`) — previously silently parsed as a `>` comparison — `getline`, command pipes, user-defined `function`s, and multi-dimensional subscripts `a[i,j]` now each error with a hint pointing at the kaish alternative.
- **`awk` `match()` sets `RSTART` and `RLENGTH`** (1-based start / match length, `0` and `-1` on no match), so the `substr($0, RSTART, RLENGTH)` extraction idiom works.
- **`awk` numeric builtins `int()` and `sqrt()`** are implemented. The rest of the math set (`sin`/`cos`/`atan2`/`exp`/`log`/`rand`/`srand`) errors with an honest "not supported" message — kaish awk is a text-processing subset.

### Changed
- **BREAKING: the `mcp()` config presets are renamed to `agent()`.** `KernelConfig::mcp()` → `KernelConfig::agent()`, `KernelConfig::mcp_with_root()` → `agent_with_root()`, `OutputLimitConfig::mcp()` → `OutputLimitConfig::agent()`, `IgnoreConfig::mcp()` → `IgnoreConfig::agent()`. These are the sandboxed-agent preset (sandboxed VFS, non-interactive, bounded memory/output) — never MCP-specific — and the old name was a misnomer after the `kaish-mcp` drop. Embedders update the call name only; behavior is identical. Parallels the existing `KernelConfig::repl()`.
- **`sed` rejects the BRE escapes that silently mis-behave under ERE**: kaish sed is *always* ERE, so BRE `\(…\)` capture groups, `\|` alternation, and `\{N,M\}` intervals used to match the wrong thing with no error. They now error with a hint to the ERE form (`(…)`, `a|b`, `a{2,5}`), and a pattern-side backreference (`\1` in the pattern) gets a sed-specific message instead of the raw engine error. `\+`/`\?` are left alone — they're valid ERE escapes for a literal `+`/`?`.
- **`awk` bare `length`** (no parentheses) now means `length($0)`, matching awk; it previously read as an unset variable (empty).
- **`awk` numeric output matches awk's `OFMT`**: integral values print in full (`100000000000000000`), non-integral values use `%.6g` (6 significant figures, e.g. `sqrt(2)` → `1.41421`). Previously fractions printed with 6 decimal places and large integers were truncated.

### Removed
- **BREAKING: the `kaish-mcp` crate (the MCP server) is removed.** kaish refocuses on being an embeddable shell library with a reference REPL; the MCP surface now lives in the embedders. The showcase is [kaibo](https://github.com/tobert/kaibo), a read-only codebase-analysis MCP that drives kaish, and [kaijutsu](https://github.com/tobert/kaijutsu) embeds kaish behind its own MCP interface. To run kaish as an MCP tool, embed the kernel and expose `execute()` — see `docs/EMBEDDING.md`. The `rmcp`/`opentelemetry-otlp` dependencies and the `kaish-mcp` binary are dropped with it.
- **BREAKING: the `git` builtin and `GitVfs` backend are removed**, and with them the `git` cargo feature and the entire `kaish-tools-git` crate (along with its `git2`/libgit2 dependency). Git is a complex, heavyweight subsystem that doesn't belong in kaish core: run your system `git` as an ordinary external command instead (`git status`, `git log` work via the `subprocess` capability). Embedders lose `GitVfs` and the `FileStatus`/`StatusSummary`/`LogEntry`/`WorktreeInfo` re-exports; the `full`/`native` feature aliases no longer include `git`. (`docs/EMBEDDING-GIT.md` is removed.)
- **BREAKING: the `rg` builtin is removed.** kaish's 80%-rule search surface is the single `grep` builtin; the separate ripgrep-flavored `rg` was redundant and carried the registry's largest flag surface. Use `grep` (recursive, gitignore-aware) instead — or your system's real `rg`, which now runs as an external command. Accepted lost features (rg-only, not ported to `grep`): `--type`/`-t`/`-T` (file-type filters), `--hidden`, `--max-count`, `--files`, `-P`/`--pcre2` (PCRE2 lookarounds/backrefs), and `--no-ignore`. The optional `pcre2` build feature and its `grep-pcre2` dependency are dropped with it.

### Fixed
- **`patch` follows `patch(1)` output** — it no longer prints a kaish-specific `N changes applied` summary line after `patching file <name>`.
- **`patch`, `write`, and `kaish-validate` read piped stdin** (`cmd | patch`, `cmd | write f`, `cmd | kaish-validate`). They read the buffered stdin field directly and so were starved by the lazy process-stdin pipe; they now drain it via the shared stdin reader like every other stdin-consuming builtin.
- **`tac`, `base64` (encode), and `xxd` now newline-terminate their output** (builtin-sweep trailing-newline policy), matching the consensus and kaish's line tools. `base64 -d` and `xxd -r` (decode/reverse) still emit raw bytes verbatim with no added newline.
- **`tr -c`/`-C`/`--complement` is now supported** — the common idiom `tr -cd '[:digit:]'` (keep only digits) used to be a clap parse error. `-c` complements SET1 across delete, translate, and squeeze modes.
- **`wc` single-count output is the bare number + newline** (`wc -l` → `2\n`) instead of a leading-tab, newline-less `\t2`; multi-count output is right-justified to a common width, single-space separated, newline-terminated. `wc -l` now counts newline characters (an unterminated final line is not counted: `a\nb` → 1, matching GNU).
- **`jq -c` (compact) now emits single-line JSON** instead of silently pretty-printing — the flag was parsed but ignored. `jq` output is also now newline-terminated (each value ends in `\n`, matching real jq and the builtin-sweep trailing-newline policy).
- **`cut -f` passes through a line that lacks the delimiter** instead of emitting an empty line (silent data loss). `cut -d, -f2` on a line with no comma now prints the whole line, matching GNU. The new `-s`/`--only-delimited` flag suppresses non-delimited lines.
- **`split --limit=N` caps the result at N fields** (N-1 splits, remainder intact) instead of N+1 — `split "a:b:c:d" ":" --limit=2` now yields `a` + `b:c:d`. Applies to literal, regex, and whitespace splitting. `split` also now emits a trailing newline (builtin-sweep trailing-newline policy).
- **`tail -n -N` and `tail -c -N` (explicit "from the end" sign) emit the last N lines/bytes** instead of the whole input. The negative count was cast to `usize`, wrapping to ~`usize::MAX`, so the skip computation silently passed everything through — `tail -n -2` printed the entire file. Both now mean the same as the plain `tail -n N` / `tail -c N` forms, matching `tail(1)`.
- **`tail -n +N` starts at line N** (1-based, "from line N to the end") instead of being silently treated as `tail -n N` (last N lines). The leading `+` was dropped when the count was parsed; `tail -n +2` now skips the first line, `tail -n +1` emits the whole input. Applies to single-stream and multi-file tail.
- **`head -n -N` prints all lines but the last N** instead of emitting everything. The negative count wrapped (`-1 as usize` → a giant `take`), so `head -n -1` silently passed the whole input through. Applies to streaming, single-file, and multi-file head (the streaming fast-path falls back to a buffered read since "all but last N" can't early-terminate).
- **External commands now receive piped stdin in any pipeline position**, streamed byte-for-byte. A non-first external stage (`seq 3 | /bin/cat`, `sort | /usr/bin/uniq`) previously got *empty* stdin because the external path read only the buffered-string stdin and ignored the streaming pipe — silent data loss. The pipe is now copied to the child concurrently (no pre-drain), so it also can't deadlock a still-producing upstream stage.
- **`cat` concatenates multiple files byte-verbatim** instead of inserting a newline between them: `cat a b` no longer adds a blank line after a newline-terminated file, and `cat x y` where `x` lacks a trailing newline now yields `xy` (joined) rather than `x\ny` — the old separator silently corrupted the byte stream.
- **`awk` lexes a regex pattern that starts a new rule** after a `}`, `;`, or newline (e.g. `/a/{print} /b/{print}`) — previously the leading `/` was mis-lexed as division (`unexpected token: Slash`). Division after a value (`x/2`, `$1/2`, `(a+b)/c`) is unaffected.
- **`awk` `split()` now populates the array** (it previously returned the field count but left the array empty — silent data loss). Fills 1-indexed keys, clears the target first, honors the whitespace/single-char/regex separator rules, and returns 0 on an empty input string (matching gawk).
- **`awk` `sub()`/`gsub()` now actually substitute** (they were silent no-ops returning 0). First-match (`sub`) / all-matches (`gsub`), `&` expands to the matched text and `\&` is a literal `&`, the result is written back to the target (`$0` by default, or a field/variable/array element), and the replacement count is returned. Writing a field rebuilds `$0` and raises `NF` when the field index extends the record. `match()`-style empty patterns don't loop.
- **`awk -F:` (and `--field-separator=:`) no longer errors at the parse layer**: the lexer now fuses a span-adjacent colon (a run of them, so `-F::` works too) directly onto a short flag into one `ShortFlag` token, so the canonical `awk -F: '{print $1}' /etc/passwd` parses and runs. Only `:` fuses — `;` and `|` are shell operators (a statement terminator and a pipe) and stay separate, exactly as in bash, so `awk -F';'`/`awk -F ';'` is how you get those separators. Bare `:` in argument position is also now a valid literal expression (for `--field-separator=:`), mirroring the existing `,` handling.
- **`sed -e EXPR -e EXPR` applies every expression**: repeated `-e` flags are now accumulated and applied in order instead of silently keeping only the last one (a "never silently corrupt" violation). `sed -e 's/a/b/' -e 's/c/d/'` chains both substitutions. The same fix corrects `sed -e EXPR file` reading the file (it previously misrouted to stdin). Repeatable value flags are now a first-class, opt-in schema property (`Vec<_>` value flags reflect as repeatable), so the kernel accumulates rather than overwrites — the mechanism generalizes to any future repeatable flag.
- **`sed 's/x/Y/2'` no longer silently ignores the occurrence count**, and `sed 's/a/b/;…'` no longer silently drops everything after the first `;` — both previously produced wrong output with no error.
- **`sed` `c` (change) on a range emits its text even when the range never closes**: `sed '2,/nomatch/c X'` (or a numeric end past EOF) previously deleted to end-of-input and emitted nothing — silent data loss. The replacement is now emitted once at EOF.
- **`sed` single-line numeric ranges span exactly one line**: `sed '2,2d'` (or any `N,N`, and descending `N,M` with `M ≤ N`) no longer also affects the following line.
- **`sed --expression=A --expression=B` applies both**: the `--flag=value` form now accumulates repeatable flags like the `-e` space form instead of silently keeping only the last (and mixing the two forms no longer clobbers or errors).
- **`awk` string/number comparison follows POSIX numeric-string (strnum) rules**: a field/`split()`/`-v`/array-subscript value compares numerically only when it *looks* like a number, while a string constant never does. `$1 == 0` with `$1="abc"` is now a string compare (`false`), not a silent `"abc" → 0.0` coercion (`true`); `$1="0"` still compares numerically. A field like `"0"` is also now false in a boolean context (`if ($1)`), and a referenced-but-absent field (`$5` on a 2-field line) is the string `""` so `$5 == 0` is false — both matching awk. (The old comparison forced numeric whenever *either* side was numeric.)
- **`awk length(arr)` returns the element count** instead of `0` — an array name was evaluated as a scalar (→ empty → length 0). Bare `length`/`length($0)` and `length(string)` are unchanged.
- **`awk exit N` sets the builtin's exit code** (`awk 'BEGIN{exit 3}'` → exit 3); the code was previously dropped and awk always exited 0. Prior output and `END` blocks still run, `exit` inside `END` overrides the code, and a *bare* `exit` (no argument) keeps the most recently set status rather than resetting it to 0 (matching awk).
- **`awk -v NAME=VALUE` (and `-v a=1 -v b=2`) now work in the natural unquoted form**, and every `-v` is applied (not just the last). `NAME=VALUE` lexes as an assignment token, which a value-taking flag previously skipped — so `-v` grabbed the *program* instead and the assignment leaked out as a stray argument (it only worked when the value was quoted). A single-value flag now consumes a following `key=value` token as its argument (getopt semantics), which also fixes the same class for other builtins' value flags; multi-value flags (`jq --arg NAME VAL`) are unaffected.
- **`awk $0 = expr` re-splits the record** into `$1..$NF` and resets `NF` (direct `$0` assignment previously left the fields and `NF` stale; `sub`/`gsub` already worked around it).
- **`awk` errors loudly on an invalid multi-character `FS` / `split()` separator regex** instead of silently falling back to a literal-string split (which silently miscounts fields). A single-character `FS` stays a literal separator (`-F '['` splits on `[`), matching awk.
- **`awk` (file input), `jq` (file input), and `head` (line-mode stdin) reject non-UTF-8 input loudly** instead of lossily replacing bad bytes with `U+FFFD`. `head` on binary stdin and `jq` on a binary file now error with a clear message rather than corrupting data (the strict-decode policy already applied to most stdin paths).
- **`head` on empty input emits nothing** instead of a bare newline: `true | head` (and any empty pipe) produced `"\n"` because the streaming path joined zero lines and appended `\n` unconditionally.
- **`kaish-validate -w`/`--warnings` actually gates warnings**: warnings (e.g. an unknown command) now show only with `-w`; without it the script reads as `valid`. The flag was inert — the gate was the tautology `A || B || !A` (always true), so warnings always printed.

## [0.8.4] - 2026-06-14

### Added
- **`diff` operand-count validation**: `diff` requires exactly two file operands — a wrong count among literal arguments is caught at pre-execution validation (error code `E011`) instead of only at runtime, and a glob/variable expansion to three or more files now errors loudly rather than silently dropping the extra operands.
- **`jq`/`sed` pre-execution validation**: a malformed `jq` filter (`E007`) or `sed` expression (`E006`) is now reported at validation time — before any pipeline runs — instead of only failing mid-execution. Filters/expressions built from variables or command substitution (`<dynamic>`) are skipped, as is a `jq` filter using `--arg`/`--argjson` bindings.
- **Glued short-flag values**: coreutils idioms like `cut -f1`, `head -c5`, `tail -c20`, `cut -f1-3`, and `grep -A1` now parse — the tail of a multi-char short-flag token binds as the flag's value when the leading char is a declared value-taking flag. A run of bare bool flags (`ls -la`) still splits per character. Generic at the arg binder, so every clap builtin benefits.
- **Bare comma in argv**: a lone `,` is accepted as the literal `","` in argument position (`cut -d, -f2`, `cut -d , -f2`, `tr -d ,`), mirroring `.`/`..`/`~`. Comma-touching runs (`echo 1,2,3`) still hit the no-token-pasting guard.

### Fixed
- **MCP suppresses no-op resource notifications**: `notifications/resources/list_changed` is now sent only when the VFS resource list actually changes between `execute` calls, instead of after every call.
- **Explicit-dot glob patterns match dotfiles**: the file walker no longer skips every `.`-leading entry before applying the pattern, so `.*`, `.github/*`, and `**/.env` reach dotfiles (bash no-`dotglob` semantics) — a `.` is matched only by a pattern segment that explicitly begins with a literal `.`, while `*`/`**`/`?`/`[…]` skip dotfiles. Previously these patterns matched nothing and shell expansion hard-errored `no matches`.
- **`grep`/`ls` report bad explicit operands**: a named operand that can't be read (`grep p real.txt typo.txt`, `ls real.txt gone.txt`) now writes `grep: typo.txt: …` / `ls: cannot access 'gone.txt': …` to stderr and exits nonzero (grep 2, ls 1), instead of silently dropping it. Recursive walks (`grep -r`) and glob expansions (`ls *.rs`) keep their benign skip-on-race tolerance.
- **`spawn --command true` keeps its value**: a value-taking flag given a bool literal (`true`/`false`) no longer has that value moved into the flag set, which had left `--command` empty and triggered clap's "a value is required". Bool *flags* still flagify as before.
- **Orphaned job files pruned on startup**: `JobManager` removes stale `kaish/jobs/` output files left by dead sessions (Linux liveness check via `/proc`), so background-job scratch files no longer accumulate. The scan runs once per process, off the job hot path.

## [0.8.3] - 2026-06-14

### Added
- **Binary data (`Value::Bytes`)**: first-class binary type throughout the pipeline — `Value::Bytes(Vec<u8>)` replaces the vestigial `Value::Blob` variant; base64 envelope on `--json`/MCP wire, xxd-style hex dump in the REPL.
- **`dd` builtin**: `if=/of=/bs=/count=/skip=` operands with k/M/G suffixes, 256 MiB transfer cap, reads via the VFS byte-range path so it terminates on endless devices, emits a `Bytes` result when no `of=` is given.
- **DevFs (`/dev/null`, `/dev/zero`, `/dev/urandom`, `/dev/random`)**: software-backed in Sandboxed/NoLocal modes; `/dev/null` discards writes and reads empty; `/dev/zero` yields exactly N zeros on counted reads; `/dev/urandom`/`/dev/random` use `getrandom` (CSPRNG); `cat /dev/zero` is a loud error naming `head -c` rather than hanging.
- **Streaming file reads**: `wc`, `cmp`, `grep` (single-file), `cat` (pipeline path), and `checksum` scan in 256 KiB windows via `read_range` — bounded memory, early-exit on broken-pipe, no full-file slurp.
- **`date` GNU rewrite**: `-d`/`--date` natural-language parser (now/today/yesterday/tomorrow, ±N units, next/last weekday, ISO absolute, nested offset idiom); `-I`/`--iso-8601`, `-R`/`--rfc-2822`, `--rfc-3339`; `-r FILE` mtime via VFS; `--json` bundle; `--tz`/`$TZ`/`-u` timezone chain; `%N`/`%3N` nanosecond translation; unknown format specifiers exit 2, not silent.
- **`ls -lR --json` children**: `OutputData::to_json` recurses into child nodes, so recursive long listings include file entries and sizes instead of collapsing to name-only objects.

### Changed
- **BREAKING:** `FOO=bar cmd` scopes assignments to that one command's environment (like bash) — assignments no longer persist past the command. A bare `FOO=bar` with no trailing command still sets the variable persistently.
- **Binary integrity through external commands**: external command stdout is captured as raw bytes (text when valid UTF-8), so `curl`, `gzip`, `base64` pipelines round-trip binary intact. Binary stdin is forwarded raw.
- External command binary output is preserved through `$()`, function bodies, and pipeline accumulation; `for x in $(binary-producer)` is now a loud error instead of iterating once over garbled text.

### Fixed
- **`MemoryFs`/`OverlayFs` streaming**: override `read_range` so chunked scans don't re-clone the entire file per window (was O(n²) on in-memory backends).
- **Text tools reject binary input**: `grep`, `sed`, `awk`, `sort`, `uniq`, `cut`, `tr`, `jq`, `tac`, `split`, `scatter`, `gather`, `read`, `tokens` error loudly on non-UTF-8 input instead of lossy-decoding with `U+FFFD`.
- **`grep` NUL handling**: falls back to whole-buffer (quiet stop) on a NUL byte rather than erroring, matching `grep-searcher` semantics; the chunk loop early-exits after a match with `-q`.
- **`dd` overflow**: `skip*bs` uses `checked_mul` (was `saturating_mul`, which could silently produce a wrong `ReadRange`).
- **`--json` on structured `.data`**: `apply_output_format` serializes the typed `.data` payload rather than re-wrapping rendered text as a JSON string — `jq '.a'` on `{"a":1}` returns the number, not the string `"1"`.
- **`dd if=` keyword token**: parser accepts statement-keyword tokens (`if`, `of`) as `key=` argv-assignment keys, unblocking `dd if=/dev/urandom`.
- **`glob` static-dir walk**: glob walks from the pattern's literal-prefix directory rather than `/`, so patterns rooted under hidden ancestors (e.g. `tempfile` paths like `/tmp/.tmpXXXX/*.txt`) no longer yield zero matches.
- **`kaish-last` binary preservation**: previous result stored as `Value::Bytes` rather than lossy-decoded.
- **Background job binary output**: job output file notes `[binary output: N bytes]` instead of logging garbled bytes.

### Removed
- `Value::Blob(BlobRef)` variant deleted — it was never constructed anywhere and only defended dead match arms.

## [0.8.2] - 2026-06-12

### Added
- **`$()` full statement grammar**: command substitution accepts `&&`/`||`/`;`/multiline/comments inside `$()`; the `Box<Pipeline>` representation is replaced by `Vec<Stmt>` with no legacy shim.
- **Command-scoped env (`Stmt::EnvScoped`)**: `FOO=bar cmd` applies prefix assignments as exported vars scoped to the body command only, then unwinds — no leak past the command (see the BREAKING note in 0.8.3).
- **Scatter newline-split**: `scatter` splits plain-text stdin on newlines (one worker per line), matching the `for $(cmd)` contract.

### Changed
- **BREAKING:** Glued argv words (`$VAR/suffix`, `/prefix/$(cmd)`) are now a parse error with a "quote the whole word" hint rather than silently splatting into multiple arguments. Single-token words (`file.txt`, `v1.2.3`) are unaffected.

### Fixed
- `grep`/`rg` value flags read from the clap `parsed` struct; the silent miss on kebab-case flags (e.g. `-A`/`-B`/`-C`) is closed.
- `kill %N` works in hermetic (no-subprocess-feature) builds via the cancellation token.
- `--json` on `test`/`[` no longer leaks the flag into the POSIX operand stream (was: "unknown binary operator: -json").
- Glob walker starts from the pattern's static-prefix directory rather than `/` (prevents zero-match on hidden-ancestor paths).
- Glob tests no longer leak `/tmp` directories — tempfile-backed tmp-builder applied.

### Removed
- **BREAKING:** Legacy `CommandSubst(Box<Pipeline>)` AST variant and the `stmt_to_pipeline` shim deleted — `$()` has one representation.

## [0.8.1] - 2026-06-11

### Added
- **OverlayFs (copy-on-write filesystem layer)**: reads prefer the upper layer, fall through to lower; first mutation snapshots the base; whiteouts hide removed lower paths; directory listings merge; `set_mtime` copies up. Inspection API: `changes()`, `reset()`, `commit_into()`, `fork_into()`.
- **`kaish-vfs` builtin**: `status` / `diff [path…]` / `commit` / `reset [path]` — inspect and finalize overlay transactions from the shell; `diff` renders unified hunks; `commit` pre-flights and resets on success with a numbered recovery procedure on partial failure.
- **`--overlay` session mode**: opt-in via `--overlay` flag (REPL + MCP) or `KernelConfig::with_overlay(true)` — all writes go into the in-memory overlay; the real filesystem is untouched until `kaish-vfs commit`. The overlay handle propagates through pipelines, forks, and scatter workers.
- **VFS byte budgets**: `KernelConfig::vfs_budget_bytes`; MCP profile defaults to 64 MiB ("vfs-memory"); `with_vfs_budget`/`without_vfs_budget` builders; `MountInfo.resident_bytes`; `kaish-mounts --json` gains a `budget` summary block. `ByteBudget` re-exported from `kaish_kernel::vfs`.
- **Suspendable timeout watchdog + `ToolCtx::patient` budgets**: movable-deadline `Watchdog` (tokio watch channel) replaces the fixed sleep; `ToolCtx::patient(budget)` returns a `PatientGuard` that freezes the script clock and governs the hold under its own budget; cancellation stays live during a hold; the `timeout` builtin is deliberately NOT suspended.
- **`kill %N` for background jobs**: per-job cancellation token cascades to builtin and external children; `kill --signal STOP/CONT/…` sends to recorded process groups. `%N` jobspec lexer token added (`wait %1`/`kill %N`).
- **`SpillMode::Memory`** forced automatically on `NoLocal` and `with_backend` kernels: large output stays memory-bounded with no host disk I/O, closing the VFS-bypass hole that disk spill left open for read-only embedders.
- **Runtime output-limit control**: `set -o output-limit=SIZE` (e.g. `set -o "output-limit=8K"`) parses and persists through the kernel's sync-back path.

### Changed
- `kaish-mounts --json` shape changed: `{mounts: [...], budget?: {...}}` (previously a flat array).
- `accumulate_result` no longer inserts an artificial newline between statement outputs — `printf "a" && printf "b"` now gives `ab` (matches bash); a trailing newline appears only when a command emits its own.

### Fixed
- `${VAR:-"default"}` — quote characters no longer leak into the expansion value.
- `! A || B` in `[[ ]]` — `!` binds tighter than `||` (was `!(A || B)`; now `(!A) || B`).
- `break N`/`continue N` preserve output printed before the signal.
- `with_backend` kernels no longer write to host `/tmp` for output spill or background-job files (side-channel closed).
- `MemoryFs::ensure_parents` TOCTOU window closed.
- `kaish-vfs status` reports session mode instead of erroring in kernels without an overlay.
- Internal hyphens in short-flag words (`-not-a-flag`) lex correctly.
- `scatter` gather line-format no longer silently drops failed workers — failures are reported and the command exits non-zero.

## [0.8.0] - 2026-06-08

### Added
- **`kaish-tool-api` crate** (new leaf): stable, auditable plugin API — `Tool`, `ToolCtx` (trimmed portable context), `KernelBackend` trait; `GlobalFlags`; `schema_from_clap`/`params_from_clap`/`validate_against_schema`. Public data types marked `#[non_exhaustive]` for semver safety.
- **`kaish-vfs` crate** (new leaf): `Filesystem` trait + `LocalFs` backend (behind `localfs`); runtime-free, builds for WASI.
- **`kaish-tools-git` crate** (new leaf): git builtin + `GitVfs` backend; `libgit2`/`git2` no longer enters the kernel build on default or localfs features.
- **`kaish-tools-host` crate** (new leaf): `ps` and host introspection tools moved out of the kernel behind the `host` feature.
- **`kaish-help` crate** (new leaf): composable help/instructions corpus; `content/en/*.md` moved here (symlinked from `docs/help`); `compose()` + `Recipe` API; `Concept`/`Variant`/`Audience`/`Depth` model; MCP tool description, MCP prompts, and REPL welcome single-sourced from recipes. `syntax.md` is generated from Syntax fragments with a drift test.
- **Per-subcommand tool schemas**: `ToolSchema.subcommands` tree + `select_leaf` literal-only routing in `build_args_async` — flags bind against the active leaf, not a flat union. `ToolSchema.aliases` on commands; required-subcommand-with-computed-positionals fails loud.
- **`owns_output` opt-out**: `ToolSchema.owns_output` — the kernel skips `apply_output_format` so a tool renders its own `--json` envelope.
- **OpenTelemetry trace context**: embedders supply W3C `traceparent`/`tracestate`/`baggage` on `ExecuteOptions`; kaish's span parents onto the embedder trace; baggage round-trips through MCP `_meta`, REPL env, and forked-span nesting.
- **Command substitution in redirect targets and heredoc bodies**: `cat < $(echo f)`, `echo x > $(echo out)` evaluate the substitution; unevaluable targets error instead of silently skipping the redirect.
- **`mktemp` entropy hardened**: `getrandom::fill` replaces the hand-rolled `/dev/urandom` + xorshift fallback; entropy failure is a hard error (no guessable names). Works on WASI.
- bash completion proof-of-life via `clap_complete`; `Kernel::try_set_cwd`/`exported_vars` accessors; REPL completion driven through the `KernelClient` trait.

### Changed
- **BREAKING:** Default feature set is now `["localfs"]`. The monolithic `native` feature decomposes into six opt-in axes: `localfs`, `subprocess`, `host`, `git`, `os-integration`, `tokens`; `full`/`native` remain as aliases for all six. Embedders must opt in to `subprocess`, `git`, `host`, etc. explicitly. `kaish-repl`/`kaish-mcp` request `full`; `kaish-wasi` stays `default-features=false`.
- **BREAKING:** `allow_external_commands` in `localfs` kernel constructors is now `cfg!(feature = "subprocess")` rather than hardcoded `true` — a localfs-only build no longer spawns subprocesses by default.
- **Clap migration complete**: all builtins moved from bespoke arg parsing to `clap_derive` across six sweep batches; `schema_from_clap` generates `ToolSchema` from the clap `Command` tree.
- `uname` node name comes from the exported `HOSTNAME` var (defaults to `"kaish"`); `--host`, `hostname`, and `~user`/`/etc/passwd` lookup gated behind the `host` feature. Hermetic `HOME`: the kernel never reads host env for `~`/`cd`/construction.
- `touch` routes `mtime` through the VFS (`set_mtime`); output redirects and stdin `< file` resolve through the VFS backend (were `std::fs`, a sandbox bypass).
- Bareword `key=value` in argv routes to `named` only for `export`/`alias`/`unalias`; everywhere else it is a positional string (matches bash). `--key=value` is always a named flag.
- `--json` output format is a kernel-level concern via `ctx.output_format`. MCP prompts now list 8 topics (added `ignore`, `output-limit`).

### Fixed
- `grep` searches all file operands, not just the first; `printf` cycles the format over extra operands (POSIX).
- Non-finite floats (`NaN`, `Infinity`) serialize to string rather than `null` in JSON output.
- `bare .` in argument position is the literal `.`, not `source` — `find .`, `ls .`, `echo .` work.
- `#` comments in the arithmetic preprocessor are skipped so apostrophes inside comments don't corrupt `$((…))`; bare backticks emit a dedicated lexer error with a `$(cmd)` hint.
- Clap-migration fallout: underscore id leak, multi-positional index mismatch, and five schema-fidelity/positional-index bugs; undeclared space-form flags to backend tools now fail loud.

## [0.7.0] - 2026-05-25

### Added
- **`rg` builtin** — ripgrep-backed search with full type filters (~100 built-in types), per-root VFS/host dispatch, stdin search, and `--files` mode; shares the new grep engine.
- **`kaish-last` builtin** — reads the previous command's structured `.data` as JSON (or its captured stdout); replaces the removed `${?.field}` syntax.
- **`for`-loop newline splitting** — `for line in $(cmd)` iterates one element per line when the substitution returns plain text, closing the `for line in $(cat file)` gap without implicit word-splitting elsewhere.
- **Cancellation cascade** — one `CancellationToken` propagates from the embedder through the kernel, forked sub-kernels, and spawned external children; SIGTERM → `kill_grace` → SIGKILL on cancel/timeout.
- **pidfd-bound child kill (Linux 5.3+)** — signals the direct child via `pidfd_send_signal`, eliminating the PID-reuse race between `waitpid` and `kill`.
- **Hermetic subprocess environment** — external commands see only variables kaish has explicitly exported; the kernel no longer inherits `std::env::vars()`. Embedders populate `KernelConfig::initial_vars`.
- **`execute_with_vars` / per-call vars overlay** — a transient variable frame scoped to one call (bash function-local semantics), popped reliably on panic via `VarsFrameGuard`. **Per-call `cwd` override** via `ExecuteOptions::cwd` with an RAII `CwdGuard`.
- **`$$` is now a kaish-session monotonic counter** — a per-kernel u64 counter instead of the host OS PID (meaningless when kaish is embedded or a long-lived MCP server).
- **REPL heredoc continuation prompt** — an unterminated heredoc triggers a continuation prompt instead of a syntax error.

### Changed
- **BREAKING:** `${?.field}` removed — use `kaish-last | jq '.field'`. The validator emits E015 with a `kaish-last` hint on lingering uses.
- **BREAKING:** `$?` returns the POSIX integer exit code only — structured data from the previous command is accessed via `kaish-last`.
- **`grep` engine replaced with `grep-searcher`** — adds `-U`/`--multiline`, `--encoding`, `--binary`; `grep --json` exposes `path`, `line_number`, `byte_offset`, `line_text`, and `submatches[{text,start,end}]`.
- **`ExecuteOptions` unifies execute variants** — `execute_with_options(input, opts)` (+ streaming form) replace the old `execute_with_vars`/`execute_streaming` family (kept as `#[deprecated]` shims).
- **`[[ ]]` comparison semantics** — `==`/`!=` use string equality for mixed types (matches bash); `-eq`/`-ne`/`-gt`/`-lt`/`-ge`/`-le` coerce operands numerically and error on non-numeric input.
- **Walker hardening** — `WalkOptions` gains `max_filesize`, `min_depth`, type filters; ancestor `.gitignore`/`.ignore`/`.rgignore` rules are loaded with rebased anchors.

### Fixed
- `sleep` responds to cancellation mid-sleep (exits 130).
- Digit-leading tokens (`019dda1c`, UUIDs) and dot-prefixed tokens (`.parent`, `.gitignore`) lex as single bareword argv tokens.
- Heredoc bodies preserve `\r` bytes verbatim; unterminated heredocs emit `LexerError::UnterminatedHeredoc` instead of silently truncating.

### Removed
- **BREAKING:** `${?.field}` syntax — replaced by `kaish-last`.
- Silent JSON sniffing of external command stdout — `.data` is opt-in via `success_with_data`; `return`/`exit` with a non-numeric string now errors instead of silently returning 0.
- Dead `BinaryOp` comparison variants never emitted by the parser, plus their eval arms and 16 inline unit tests.

## [0.6.0] - 2026-04-18

### Added
- **`kaish-wasi` crate** — `wasm32-wasip1` binary that reads scripts from stdin and writes `ExecResult` JSON to stdout; runs a `NoLocal` sandboxed kernel with no external commands. Verified with wasmtime.
- **`native` feature gate** — OS-specific deps (git2, trash, directories, tiktoken-rs, nix, procfs, tokio process/signal/fs) gated behind a default-on `native` feature; disabling it yields a pure-Rust sandbox.
- **`tac`, `base64`, `xxd`, `checksum`, `timeout` builtins** — line reverse; base64 encode/decode; hex dump/reverse; sha256/sha1/md5 with verify mode; deadline runner (exit 124).
- **Here-string `<<<`** — feeds the expanded word plus trailing newline to stdin; multiple stdin-source redirects on one command are a parse error.
- **`jq --arg`/`--argjson`/`-n`** — binds shell variables into jq filters using real jq CLI vocabulary (`consumes: 2` arity).
- **`Kernel::fork()`** — per-worker sub-kernel with snapshotted mutable state and shared read-only resources; background jobs, scatter workers, and concurrent pipeline stages each fork independently.
- **`ExecContext.dispatcher`** — builtins (e.g. `timeout`) re-dispatch inner commands through the full resolution chain (user tools → builtins → `.kai` scripts → external).
- **`ExecResult.baggage`** (W3C-Baggage context propagation) and **`ExecResult.content_type`** (MIME hint surfaced by the MCP handler).
- **`jq`/`cut` structured iteration** — both populate `ExecResult.data` as a JSON array so `for v in $(…)` iterates per element. Heredoc span tracking for validator pinpointing.

### Changed
- **BREAKING:** `kaish-mcp` is now server-only — the MCP client (wrapping external MCP servers' tools as builtins) is removed; `ExternalServerConfig`/`mcp_servers` deleted; `rmcp` is `default-features = false`.
- **`ExecResult.out`/`.output` fields are now private** — access via `text_out()`, `output()`, `set_out()`, `push_out()`, `take_output()`, `materialize()`, `from_parts()`, `with_code()`, etc.
- **`execute()` serialised per-kernel** — concurrent callers on the same kernel queue through a fair `tokio::sync::Mutex`; contention logs a `tracing::warn!`.
- **`timeout` dispatches through the full resolution chain** — previously reimplemented spawning, missing output limits, VFS redirects, and user-tool resolution.
- Table headers normalised to UPPERCASE across `ls`/`wc`/`stat`/`jobs`/`grep`/`head`/`tail` (consistent with `ps`/`vars`/`tools`/`mounts`); `ToolArgs::named` uses `BTreeMap` for deterministic order. Relative paths like `src/kaish` lex as a single `RelativePath` token.

### Fixed
- `test -r/-w/-x/-e/-f/-d` route through `ctx.backend.stat()` (VFS-aware), closing a sandbox-bypass probe of the host FS; test flags reconstructed from flags + positional so they are no longer eaten by the arg parser.
- `head -c`/`tail -c` count bytes (POSIX), not Unicode chars; `cat` with no args/stdin exits 0 with empty output.
- `MemoryFs::rename("a", "a/b")` returns `InvalidInput`; `rename("a","a")` is a no-op; `ensure_parents` rejects a file where a directory is expected.
- `accumulate_result` propagates `did_spill`/`original_code`/`content_type`/`baggage`; for-loop body and `&&`/`||` errors no longer leak scope frames or leave `errexit` suppressed.
- `patch` validates hunk counts against actual diff lines; VFS symlink-escape check does a canonical-path comparison; heredoc preprocessing preserves trailing newlines.

## [0.5.0] - 2026-03-14

### Added
- **Bare glob expansion** in argument and for-loop positions: `*.txt`, `src/**/*.rs`, `file?.log`, `[a-z].txt` expand to matching files before dispatch. Zero matches is an error (zsh-style). Toggle with `set -o glob` / `set +o glob` (default on).

### Changed
- Quoted patterns (`"*.txt"`) and variable content (`$VAR`) are never re-globbed.

### Removed
- Validator error E013 (ShellGlobPattern) — bare globs are now valid syntax, not a pre-execution error.

## [0.4.0] - 2026-03-10

### Added
- `--init <path>` for `kaish-mcp`: loads `.kai` init scripts before each `execute()`, re-read from disk every call so edits take effect without restart.
- `configure_tools` closure on `Kernel::with_backend()` — embedders register or shadow builtins at construction time. `ExecContext::verify_nonce()`/`latch_result()` helpers for nonce-based confirmation in custom builtins.
- **`kaish-confirm <nonce>` builtin** — a language-level command to confirm latched operations.
- **Spill signaling** — output-limit truncation exits 3 (terminal spill) or 2 (latch nonce available) instead of silently exiting 0; `ExecResult.data` carries structured latch state (`nonce`, `command`, `paths`, `hint`, `ttl`). `set -o output-limit[=SIZE]` for in-session configuration.

### Changed
- **BREAKING:** `--confirm=<nonce>` kernel escape hatch removed — use `kaish-confirm <nonce>`.
- **BREAKING:** Output-limit spill no longer exits 0 — agents relying on silent truncation now see exit 3. MCP default output limit lowered 64 KiB → 8 KiB.
- `ExecResult` canonical output is materialized lazily from `OutputData` (`text_out()`); `with_output_and_text()` for builtins needing a custom pipe representation. `ls --json` stores raw numeric sizes (alignment is the renderer's job); `ls -lR` returns `OutputData::table` with headers.

### Fixed
- **Pipeline deadlock** when pipe output exceeds 64 KiB — producer sends the structured-data oneshot before blocking on the pipe write; consumer starts dispatch before awaiting it.
- `$(cmd)` inside `[[ ]]`, `case`, `return`, `exit` executes via `eval_expr_async` (was the sync `NoOpExecutor`, silently empty).
- `[[ -f/-d/-e ]]` route through the VFS backend (`eval_test_async`), working with MemoryFs/JobFs/etc.
- POSIX multi-char short flags (`find -name`, `-type`) consult the tool schema before splitting into single-character flags.
- OOM-safe spill estimates `OutputData` size before materializing and streams large output (and `>`/`>>` redirects) directly to disk. Unquoted `::`/colon words (`host:8080`, `std::fs::File`) lex correctly.

## [0.3.4] - 2026-03-04

### Fixed
- Silent builtins (`cd`, `touch`, `rm`, `mkdir`, `mv`, `cp`, `ln`, and others) no longer print a blank line after each REPL invocation — they return `ExecResult::success("")` and the REPL emits `ProcessResult::Empty`. Removed the startup blank line before the first prompt.

## [0.3.3] - 2026-03-02

### Fixed
- `rm` trash exclusion incorrectly matched paths like `/tmp_file.txt` against the `/tmp` prefix via string `starts_with`; fixed to use `Path::starts_with` for component-boundary matching.

## [0.3.2] - 2026-03-02

### Added
- **`kaish-types` crate** — pure-data leaf crate extracted from `kaish-kernel` (`OutputData`, `ExecResult`, `Value`, `DirEntry`, `ToolSchema`, `ToolArgs`, `BackendError`, `JobInfo`, …). No async runtime, parser, or I/O; depends only on `serde`/`serde_json`/`thiserror` (+ optional `schemars`). All 70+ builtin files resolve unchanged via re-exports.
- **Confirmation latch** (`set -o latch` / `KAISH_LATCH=1`) — `rm` returns exit 2 with a nonce on first call; confirm with `rm --confirm=<nonce> <path>` (60 s TTL, idempotent).
- **Trash-on-delete** (`set -o trash` / `KAISH_TRASH=1`) — small files (≤10 MiB default) move to the freedesktop.org Trash; large files bypass trash and are gated by latch; directories always trash. `kaish-trash` builtin: `list`/`restore`/`empty` (always requires a nonce)/`config`.
- **`NonceStore`** Arc-shared across MCP `execute()` calls so latch nonces survive between requests; scoped to `{command, paths}`. `KernelConfig::allow_external_commands` flag gates all external execution paths.
- **Cancellation API** — `Kernel::cancel()`/`is_cancelled()`/`reset_cancel()`; REPL wires SIGINT via `tokio::select!`; checkpoints in statement/`for`/`while` loops; exit 130 on cancel. MCP `outputSchema` and tool annotations (`destructive`, `non-idempotent`, `open-world`).

### Changed
- **BREAKING:** `validate`, `vars`, `tools`, `mounts` renamed to `kaish-validate`, `kaish-vars`, `kaish-tools`, `kaish-mounts` (no back-compat aliases; convention in `docs/NAMING.md`).
- `kaish-schema` crate (Cap'n Proto IPC/RPC) removed; `IpcClient`/`KernelRpcServer` deleted.
- `set -e` no longer triggers on the left of `&&`/`||` (bash-compatible). `block_in_place` removed from hot paths (fixes MCP `current_thread` runtime panics).

### Fixed
- Stderr from pipeline stages inside control structures drains incrementally; stage panics report all stages, not just the last.
- Trash failure is exit 1 with an actionable message (was a silent fall-through to permanent delete). Path-scoped nonce `validate()` checks both command and authorized-path subset.

## [0.3.1] - 2026-02-23

### Added
- `lstat()` on the `KernelBackend` trait (local, overlay, test backends) and `is_dir()`/`is_file()`/`is_symlink()` predicates on `DirEntry`.

### Changed
- **BREAKING:** `vfs::Metadata`, `vfs::DirEntry`, and `backend::EntryInfo` unified into one `DirEntry` (carrying `modified`, `permissions`, `kind`, path); `stat()`/`lstat()` return it directly. `EntryType` renamed `DirEntryKind`; field `entry_type` renamed `kind`.

### Fixed
- `test -L` / `test -h` detect symlinks via `lstat()` (was hardcoded to false).
- External command dispatch no longer errors when the CWD is a virtual path — it falls through to backend-registered tools as intended.

## [0.3.0] - 2026-02-17

### Added
- **Streaming pipelines** — concurrent stages connected by bounded 64 KiB `PipeStream` ring buffers with backpressure; external stdout streams in 8 KiB chunks. `cat`/`grep`/`head` are streaming builtins; all others migrated to async `read_stdin_to_string()`.
- **Signal handling and Unix job control** — `fg`/`bg`/`kill` builtins, `Ctrl-Z` suspend, child process groups.
- **`alias`/`unalias`** — text-level first-word expansion (non-recursive), resolving in scripts and `$()` as well as the REPL.
- **RC/init loading** (`$KAISH_INIT`, `~/.config/kaish/init.kai`, `~/.kaishrc`) and a `kaish_prompt()` dynamic-prompt hook.
- **`/v/bin` virtual directory** exposing every builtin; absolute/relative path commands (`/bin/echo`, `./script.sh`, `../tool`) execute directly. Path normalization resolves `.`/`..` lexically.
- MCP: `logging` capability with severity filtering; help-content prompts; `roots`/`on_roots_list_changed` mount client workspaces as VFS roots; resource-list-changed notifications; `inotify`-backed VFS resource subscriptions emitting `notifications/resources/updated`.

### Changed
- **BREAKING:** `/command` REPL meta-commands removed — use `kaish-*` builtins (`kaish-ast`, `kaish-clear`, `kaish-version`, `kaish-status`).
- `scatter` requires structured data for multi-item iteration; `spawn` no longer splits plain-string args on whitespace. Help files rewritten for token density (~45% reduction). MCP HTTP transport removed (stdio-only); `/tmp` isolated per handler.

### Fixed
- `cd ..`, `cd ~`, bare `cd`, `../path` navigation; `HOME` lookup checks shell scope before process env. UTF-8 corruption when multi-byte chars split across 8 KiB read boundaries. Command-not-found exits 127. MCP `on_initialized` async deadlock (`list_roots()` now spawned). Arithmetic preprocessor is quote-aware. Full test suite time ~145 s → ~9 s.

## [0.2.1] - 2026-02-14

### Added
- Schema-aware positional-to-named mapping (`ToolSchema.map_positionals`) for MCP/external tools; builtins keep `map_positionals = false`.
- MCP: `logging` skeleton, `roots` handling, resource-list-changed notifications (rmcp 0.15), help-content prompts, progress notifications.
- `split` stdin fallback (`echo "a,b,c" | split ","`); validator rule E014 (`scatter` without `gather`).

### Changed
- **BREAKING:** MCP HTTP transport removed (stdio-only; tunnel or proxy for remote access).
- `scatter` requires structured data; `spawn` no longer whitespace-splits plain-string args. MCP `/tmp` isolated per handler (PID-scoped). All tests migrated to ephemeral temp paths.

### Fixed
- Double newlines in for-loop and multi-statement output. `glob` sets `result.data` (JSON array) so `$()` is iterable in `for`. MCP `on_initialized`/`on_roots_list_changed` async deadlock. Test suite ~145 s → ~9 s.

## [0.2.0] - 2026-02-13

### Added
- **OpenTelemetry instrumentation** across kernel execution and the MCP server — spans flow to any OTLP collector when `OTEL_EXPORTER_OTLP_ENDPOINT` is set; W3C `traceparent` propagated outbound from MCP `call_tool` `_meta`.
- `format_string` shared module — `printf` and `awk` `sprintf` support `width`/`alignment`/`precision`. Heredoc variable expansion (`<<EOF` expands, `<<'EOF'` literal). Case patterns accept path literals and variable references.

### Fixed
- `tr [:alpha:]` includes uppercase A–Z; heredoc preserves leading empty lines; `seq` uses proportional float tolerance.
- `cut` errors on multi-char delimiters; `-d`/`-f`/`-c` registered with aliases. Pipeline continues on command failure (shell semantics; `set -e` still exits, including in loop bodies).
- `MemoryFs` symlink loop returns `ELOOP` instead of stack overflow. `LocalFs` `lstat`/`read_link` validate path containment; symlink creation rejects sandbox-escaping absolute targets.
- `exit` preserves accumulated stdout; `if`/`case` branch bodies accumulate output across statements. `[[ ]]` accepts `=` as an alias for `==`.

## [0.1.8] - 2026-02-11

### Changed
- **BREAKING:** `Kernel::with_backend`, `with_backend_and_virtual_paths`, and `with_backend_and_custom_vfs` merged into one `Kernel::with_backend(backend, config, configure_vfs)` constructor.
- **BREAKING:** `/scratch` VFS mount removed — use `/tmp` (visible to external commands).

### Added
- `execute_streaming` on `EmbeddedClient` for per-statement output callbacks.

### Removed
- TOON output format (undocumented, excess dependency weight) — `--json` is the only structured-output flag.

### Fixed
- Scope and cwd restored unconditionally after `$()`, `execute_user_tool`, and `try_execute_script` — an error return no longer leaks variable assignments or `cd` side effects into the parent.

## [0.1.7] - 2026-02-11

### Added
- `CommandDispatcher` trait + `PipelinePosition` enum unify single-command and pipeline execution through one code path.
- Schema-aware short-flag value binding (`head -n 5`).
- MCP `execute` returns clean readable text by default (plain text passes through; structured data emits canonical TSV/newline; `--json` forces JSON).

### Fixed
- `$()` snapshots/restores scope and cwd so assignments and `cd` inside it don't leak. Scatter/gather workers route through `CommandDispatcher`. `PipelinePosition::Only` assigned for single-command pipelines. Scope frames are Arc copy-on-write (O(1) snapshot/restore).

## [0.1.6] - 2026-02-09

### Added
- MCP responses include both a plain-text `content` block and a full `ExecuteResult` JSON `structured_content` block; `is_error` set explicitly.

### Fixed
- README accuracy pass and docs consistency check added to the release workflow.

## [0.1.5] - 2026-02-09

### Added
- `/release` slash command covering the full publishing lifecycle (pre-flight, code review, version bump, commit, tag, push, publish in dependency order).

### Fixed
- Help topic content and builtin categorization corrected against the real builtin set (16 orphaned builtins categorized, 10 ghost entries removed, `ls -l` typo, accurate `scatter`/`gather` parameter table).

## [0.1.4] - 2026-02-09

### Added
- Streaming output for the script runner (`kaish script.kai`) and `-c` mode — external commands inherit stdio for real-time output; builtins flush per-statement.

### Fixed
- Help docs moved into the `kaish-kernel` crate source tree so `cargo install` packaging works.

## [0.1.3] - 2026-02-09

### Added
- **BREAKING:** `exec` builtin renamed to `spawn` (subprocess with output capture, env/cwd/timeout). A new `exec` provides POSIX process replacement via `execvp` (never returns on success).
- MCP protocol bumped to `2025-03-26`; `list_resource_templates()` exposes the `kaish://vfs/{+path}` URI template. Publish scripts for crates.io releases.

### Fixed
- Script runner inherits the caller's real working directory with passthrough FS instead of defaulting to `$HOME` with a sandboxed VFS.

### Changed
- Help docs consolidated under repo-root `docs/help/`; dependencies bumped.

## [0.1.2] - 2026-02-08

### Added
- Install section, contributing guidelines, and sister-project links in the README.

### Fixed
- Cap'n Proto generated code vendored so `cargo install` works without the `capnp` CLI. All crates point at the workspace-root README for crates.io.

## [0.1.1] - 2026-02-08

### Fixed
- Help documentation files copied into `kaish-kernel` for correct crates.io packaging (were missing from the published crate).

## [0.1.0] - 2026-02-08

Initial public release of **kaish** (会sh) — a predictable Bourne-like shell for AI agents, embeddable as a library and deployable as an MCP server.

### Added
- **Lexer + parser** (`logos` + `chumsky`): Bourne/bash-compatible syntax, arithmetic `$(( ))`, command substitution `$()`, heredocs, `elif`, regex match `=~`/`!~`, syntax-highlighting token categories.
- **Pre-execution validator** with typed error codes that catch mistakes before any command runs.
- **No implicit word splitting** — `$VAR` is always one value; `split` is explicit; `for x in $(cmd)` iterates on newlines only.
- **Interpreter** (Tokio async): pipelines, background jobs (`&`), `.kai` user functions, `if`/`for`/`while`/`case`, `set -e`/`set -o`.
- **In-process builtins** for text processing and file ops (`grep`, `jq`, `awk`, `sed`, `find`, `diff`, `patch`, `git`, `cat`, `head`, `tail`, `cut`, `wc`, `ls`, `cp`, `mv`, `rm`, `sort`, `uniq`, `tr`, `test`/`[`/`[[`, `scatter`/`gather`, `spawn`, `tokens`, and more) with a structured `OutputData` model and a global `--json` flag.
- **Virtual Filesystem (VFS)** with pluggable backends — `LocalFs` (sandboxed to `$HOME`), `MemoryFs`, `GitVfs` (libgit2), `JobFs` (`/v/jobs` observability).
- **Background jobs** (`cmd &`) observable at `/v/jobs`; **scatter/gather** parallel fan-out (`散`/`集`) with a concurrency limit. Transparent `PATH` fall-through for unknown commands.
- **`kaish-glob` crate** — standalone glob/gitignore-aware async walker with symlink-loop detection and ReDoS protection.
- **REPL** (`kaish-repl`) with multi-line input, completion, and history; **MCP server** (`kaish-mcp`) exposing `kaish_execute` with help resources and structured + plain-text content blocks.
- **`KernelClient` trait** + `EmbeddedClient` for in-process embedding; topic-based help system; `kaish-wasi` `wasm32-wasip1` target.

[Unreleased]: https://github.com/tobert/kaish/compare/v0.11.0...HEAD
[0.11.0]: https://github.com/tobert/kaish/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/tobert/kaish/compare/v0.9.1...v0.10.0
[0.9.1]: https://github.com/tobert/kaish/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/tobert/kaish/compare/v0.8.4...v0.9.0
[0.8.4]: https://github.com/tobert/kaish/compare/v0.8.3...v0.8.4
[0.8.3]: https://github.com/tobert/kaish/compare/v0.8.2...v0.8.3
[0.8.2]: https://github.com/tobert/kaish/compare/v0.8.1...v0.8.2
[0.8.1]: https://github.com/tobert/kaish/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/tobert/kaish/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/tobert/kaish/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/tobert/kaish/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/tobert/kaish/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/tobert/kaish/compare/v0.3.4...v0.4.0
[0.3.4]: https://github.com/tobert/kaish/compare/v0.3.3...v0.3.4
[0.3.3]: https://github.com/tobert/kaish/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/tobert/kaish/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/tobert/kaish/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/tobert/kaish/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/tobert/kaish/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/tobert/kaish/compare/v0.1.8...v0.2.0
[0.1.8]: https://github.com/tobert/kaish/compare/v0.1.7...v0.1.8
[0.1.7]: https://github.com/tobert/kaish/compare/v0.1.6...v0.1.7
[0.1.6]: https://github.com/tobert/kaish/compare/v0.1.5...v0.1.6
[0.1.5]: https://github.com/tobert/kaish/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/tobert/kaish/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/tobert/kaish/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/tobert/kaish/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/tobert/kaish/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/tobert/kaish/releases/tag/v0.1.0
