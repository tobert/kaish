# Changelog

All notable changes to **kaish** (会sh) are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

While kaish is pre-1.0, minor (`0.X.0`) releases may carry breaking changes;
breaking entries are marked **BREAKING**.

## [Unreleased]

### Added
- **`sed` ergonomics pass** (gaps chosen from a cross-model usability panel — see `docs/sed-design.md`): `;` now chains multiple commands in one expression (`sed 's/a/b/; s/c/d/'`); `s///N` / `s///Ng` act on the Nth match; `a TEXT`/`i TEXT`/`c TEXT` append/insert/change lines (all of `a\TEXT`, `a TEXT`, `aTEXT`); `y/abc/xyz/` transliterates; and `-E`/`-r` are accepted as no-ops (kaish sed is always ERE).

### Changed
- **`sed` rejects the BRE escapes that silently mis-behave under ERE**: kaish sed is *always* ERE, so BRE `\(…\)` capture groups, `\|` alternation, and `\{N,M\}` intervals used to match the wrong thing with no error. They now error with a hint to the ERE form (`(…)`, `a|b`, `a{2,5}`), and a pattern-side backreference (`\1` in the pattern) gets a sed-specific message instead of the raw engine error. `\+`/`\?` are left alone — they're valid ERE escapes for a literal `+`/`?`.

### Added
- **`awk` range patterns** `/start/,/end/` (and expression endpoints like `NR==2,NR==5`) — fire inclusively from the record matching `start` through the record matching `end`.
- **`awk` fails loud on unsupported constructs** instead of mis-parsing or silently doing nothing: output redirection (`print > f`, `>>`) — previously silently parsed as a `>` comparison — `getline`, command pipes, user-defined `function`s, and multi-dimensional subscripts `a[i,j]` now each error with a hint pointing at the kaish alternative.
- **`awk` `match()` sets `RSTART` and `RLENGTH`** (1-based start / match length, `0` and `-1` on no match), so the `substr($0, RSTART, RLENGTH)` extraction idiom works.
- **`awk` numeric builtins `int()` and `sqrt()`** are implemented. The rest of the math set (`sin`/`cos`/`atan2`/`exp`/`log`/`rand`/`srand`) errors with an honest "not supported" message — kaish awk is a text-processing subset.

### Changed
- **`awk` bare `length`** (no parentheses) now means `length($0)`, matching awk; it previously read as an unset variable (empty).
- **`awk` numeric output matches awk's `OFMT`**: integral values print in full (`100000000000000000`), non-integral values use `%.6g` (6 significant figures, e.g. `sqrt(2)` → `1.41421`). Previously fractions printed with 6 decimal places and large integers were truncated.

### Fixed
- **`awk` lexes a regex pattern that starts a new rule** after a `}`, `;`, or newline (e.g. `/a/{print} /b/{print}`) — previously the leading `/` was mis-lexed as division (`unexpected token: Slash`). Division after a value (`x/2`, `$1/2`, `(a+b)/c`) is unaffected.
- **`awk` `split()` now populates the array** (it previously returned the field count but left the array empty — silent data loss). Fills 1-indexed keys, clears the target first, honors the whitespace/single-char/regex separator rules, and returns 0 on an empty input string (matching gawk).
- **`awk` `sub()`/`gsub()` now actually substitute** (they were silent no-ops returning 0). First-match (`sub`) / all-matches (`gsub`), `&` expands to the matched text and `\&` is a literal `&`, the result is written back to the target (`$0` by default, or a field/variable/array element), and the replacement count is returned. Writing a field rebuilds `$0` and raises `NF` when the field index extends the record. `match()`-style empty patterns don't loop.
- **`awk -F:` (and `--field-separator=:`) no longer errors at the parse layer**: the lexer now fuses a span-adjacent colon (a run of them, so `-F::` works too) directly onto a short flag into one `ShortFlag` token, so the canonical `awk -F: '{print $1}' /etc/passwd` parses and runs. Only `:` fuses — `;` and `|` are shell operators (a statement terminator and a pipe) and stay separate, exactly as in bash, so `awk -F';'`/`awk -F ';'` is how you get those separators. Bare `:` in argument position is also now a valid literal expression (for `--field-separator=:`), mirroring the existing `,` handling.
- **`sed -e EXPR -e EXPR` applies every expression**: repeated `-e` flags are now accumulated and applied in order instead of silently keeping only the last one (a "never silently corrupt" violation). `sed -e 's/a/b/' -e 's/c/d/'` chains both substitutions. The same fix corrects `sed -e EXPR file` reading the file (it previously misrouted to stdin). Repeatable value flags are now a first-class, opt-in schema property (`Vec<_>` value flags reflect as repeatable), so the kernel accumulates rather than overwrites — the mechanism generalizes to any future repeatable flag.
- **`sed 's/x/Y/2'` no longer silently ignores the occurrence count**, and `sed 's/a/b/;…'` no longer silently drops everything after the first `;` — both previously produced wrong output with no error.
- **`sed` `c` (change) on a range emits its text even when the range never closes**: `sed '2,/nomatch/c X'` (or a numeric end past EOF) previously deleted to end-of-input and emitted nothing — silent data loss. The replacement is now emitted once at EOF.
- **`sed` single-line numeric ranges span exactly one line**: `sed '2,2d'` (or any `N,N`, and descending `N,M` with `M ≤ N`) no longer also affects the following line.
- **`sed --expression=A --expression=B` applies both**: the `--flag=value` form now accumulates repeatable flags like the `-e` space form instead of silently keeping only the last (and mixing the two forms no longer clobbers or errors).

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

[Unreleased]: https://github.com/tobert/kaish/compare/v0.8.4...HEAD
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
