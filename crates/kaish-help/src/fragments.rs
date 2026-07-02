//! The English fragment registry.
//!
//! Phase 1 seeded the composition surface with the [`Concept::Foundations`] spine
//! (the guarantees and the idioms that follow from them) plus a couple of
//! [`Concept::Model`] fragments. Phase 2 (light) adds the [`Concept::Syntax`]
//! reference sections, which are the single source for `content/en/syntax.md`
//! (a committed, drift-tested mirror). `LANGUAGE.md` stays hand-authored. See
//! `docs/composable-help.md`.
//!
//! Bodies are inline `&'static str` for now; when i18n lands they move to
//! per-locale files keyed by (concept, key, variant).

use crate::compose::{Audience, Concept, Depth, Fragment, Variant, DEFAULT_LOCALE, UNRANKED};

/// Shorthand for an inline English fragment (no section heading). Unranked by
/// default; the always-on onboarding spine attaches an importance rank with
/// [`Fragment::ranked`].
const fn en(
    concept: Concept,
    key: &'static str,
    variant: Variant,
    depth: Depth,
    audience: Option<Audience>,
    body: &'static str,
) -> Fragment {
    Fragment {
        concept,
        key,
        variant,
        depth,
        locale: DEFAULT_LOCALE,
        audience,
        rank: UNRANKED,
        title: None,
        body,
    }
}

/// Shorthand for a titled English `Syntax`-reference section. These render as
/// `## <title>` blocks in [`crate::compose::render_syntax_reference`], which is the
/// single source for `content/en/syntax.md`.
const fn syntax_section(key: &'static str, title: &'static str, body: &'static str) -> Fragment {
    Fragment {
        concept: Concept::Syntax,
        key,
        variant: Variant::Example,
        depth: Depth::Reference,
        locale: DEFAULT_LOCALE,
        audience: None,
        rank: UNRANKED,
        title: Some(title),
        body,
    }
}

/// The canonical English content set.
pub const FRAGMENTS: &[Fragment] = &[
    // ---- Model ---------------------------------------------------------------
    en(
        Concept::Model,
        "what",
        Variant::Rule,
        Depth::Summary,
        None,
        "kaish (会sh) is a Bourne-like shell for AI agents: familiar syntax, fewer \
         footguns, validated before execution. Builtins run in-process; external \
         commands run via `PATH`.",
    ),
    en(
        Concept::Model,
        "data-model",
        Variant::Rule,
        Depth::Summary,
        None,
        "Values can be structured JSON — a list or a record — not only strings. \
         Access is brackets-only (`${r[key]}`, `${xs[0]}`, never dots); iterate with \
         `$(values $c)` (elements/values) or `$(keys $c)` (indices/keys); `fromjson` / \
         `tojson` bridge JSON text in and out. See `help syntax` → Collections.",
    ),
    en(
        Concept::Model,
        "welcome",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Human),
        "Type `help` for topics, `help <tool>` for a specific builtin, and `exit` to quit.",
    ),
    // ---- Foundations: guarantees + the idioms that follow --------------------
    en(
        Concept::Foundations,
        "no-word-splitting",
        Variant::Rule,
        Depth::Summary,
        None,
        "**No word splitting.** `$VAR` is always a single value — a variable holding \
         spaces stays one argument. Use `split` when you actually want to split on \
         whitespace, a delimiter, or a regex.",
    )
    .ranked(0),
    en(
        Concept::Foundations,
        "no-word-splitting",
        Variant::Contrast,
        Depth::Reference,
        None,
        "Bash splits unquoted `$VAR` on `$IFS`; kaish never does, so the defensive-\
         quoting dance is unnecessary.",
    ),
    en(
        Concept::Foundations,
        "quote-to-join",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word \
         unless quoted — kaish never pastes adjacent unquoted tokens. To build one \
         word from text plus interpolation, wrap the whole thing in double quotes: \
         `\"$dir/file.txt\"`, `\"out-$(date +%s).log\"`.",
    )
    .ranked(1),
    en(
        Concept::Foundations,
        "quote-to-join",
        Variant::Contrast,
        Depth::Reference,
        None,
        "Bash pastes adjacent tokens, so bare `$dir/file` is one word. kaish keeps \
         them separate: bare `$dir/file` becomes two arguments, and a bare \
         interpolated redirect target (`> $dir/out`) is a parse error. Always quote \
         interpolated words — the same thing `shellcheck` (SC2086) asks for.",
    ),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Structured output.** Every builtin can emit machine-readable data with \
         `--json` (`ls --json`, `ps --json`, `kaish-vars --json`).",
    )
    .ranked(5),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Example,
        Depth::Reference,
        None,
        "```\nls --json | jq '.[].name'\n```",
    ),
    en(
        Concept::Foundations,
        "newline-split",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Newline-split substitution.** `for x in $(cmd)` splits on newlines only — \
         one iteration per line; whitespace within a line never splits.",
    )
    .ranked(3),
    en(
        Concept::Foundations,
        "structured-substitution",
        Variant::Rule,
        Depth::Summary,
        None,
        "`$(cmd)` carries structured data: `for i in $(seq 1 5)` iterates five values, \
         not split text. Enumerate a collection the same way — `for x in $(values $c)` \
         (list elements / record values) or `for k in $(keys $c)` (list indices / record \
         keys). A bare `for x in $c` is an error: wrap the collection in `$(...)`.",
    )
    .ranked(2),
    en(
        Concept::Foundations,
        "glob-strict",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Strict globs.** `*.txt` expands to matching files; zero matches is an \
         error, not a silent pass-through.",
    )
    .ranked(4),
    en(
        Concept::Foundations,
        "pre-validation",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Pre-validation.** kaish validates the whole command before running it — \
         syntax errors are caught up front, so a command never half-runs.",
    )
    .ranked(6),
    en(
        Concept::Foundations,
        "crash-not-corrupt",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Fail loud, not silent.** kaish prefers to error over corrupting data; \
         destructive operations can require a confirmation nonce via `set -o latch`.",
    )
    .ranked(7),
    en(
        Concept::Foundations,
        "json-orchestration",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Agent),
        "When orchestrating tools, prefer `--json` piped through `jq` — consuming \
         structured data beats scraping text output.",
    )
    .ranked(8),
    en(
        Concept::Foundations,
        "overlay-mode",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Agent),
        "**Overlay mode** (opt-in: `--overlay` flag or `KernelConfig::with_overlay`). \
         All writes go into a virtual in-memory layer; the real filesystem is never \
         touched until you explicitly commit. Use `kaish-vfs` to inspect and finalize \
         the transaction: `kaish-vfs status` (dirty flag + counts), `kaish-vfs diff` \
         (unified diff), `kaish-vfs commit` (write to real files), \
         `kaish-vfs reset [path]` (discard edits). \
         **Fresh-kernel-per-call rule**: when an embedder runs a fresh kernel per \
         `execute()` call (the common pattern), each call gets a fresh overlay \
         transaction. `kaish-vfs commit` MUST run in the same call as the writes — if \
         you commit in a later call the transaction from the write call was already \
         discarded.",
    )
    .ranked(9),
    // ---- Syntax reference (single source for content/en/syntax.md) -----------
    syntax_section(
        "variables",
        "Variables",
        r#"```sh
NAME="value"              # assignment (no spaces around =)
local NAME="value"        # local scope
COUNT=42                  # integer
PI=3.14159                # float
ENABLED=true              # boolean (only true/false)
```"#,
    ),
    syntax_section(
        "expansion",
        "Expansion",
        r#"```sh
$VAR                      # simple
${VAR}                    # braced
${VAR:-default}           # default if unset/empty
${#VAR}                   # string length
$0 $1 $@ $#              # script name, args, all args, count
$?                        # last exit code (0-255)
$$ ${$}                   # kaish session id — see note below
```

**`$$` is a kaish-internal session identifier**, not the OS PID. It's a
monotonic `u64` counter (starts at 1) assigned at kernel construction;
forks/subshells inherit the parent's value. This is an intentional
divergence from bash — kaish runs embedded inside long-lived host
processes, where the host PID is meaningless to the script."#,
    ),
    syntax_section(
        "collections",
        "Collections (lists & records)",
        r#"```sh
# Values are structured JSON (list or record); fromjson/tojson bridge text.
u=$(fromjson '{"name":"amy","tags":["rust","shell"]}')
xs=$(fromjson '[10,20,30]')

# READ ACCESS — brackets only, never dots. Bad access is a loud error.
${u[name]}                # record key (bareword = literal key)
${u[$k]}                  # dynamic key ($var)
${xs[0]}   ${xs[-1]}      # list index; negative counts from the end
${xs[0:2]}                # slice (end-exclusive) → a list
${u[tags][0]}             # nested path
${#xs}   ${#u}            # length: list elements / record keys

# ENUMERATE — always wrap the collection in $(keys ...) or $(values ...).
# A bare `for x in $xs` is an ERROR (E012): there is no word splitting.
# keys → indices (list) / keys (record).  values → elements (list) / values (record).

for x in $(values $xs); do echo $x; done          # each list element: 10 20 30
for i in $(keys $xs);   do echo $i; done          # each list index:   0 1 2
for k in $(keys $u);    do echo $k; done          # each record key:   name tags
for v in $(values $u);  do echo $v; done          # each record value

# key + value together — index the record by the loop key:
for k in $(keys $u); do echo "$k = ${u[$k]}"; done

# nested — a subscript access is still a VarRef, so it needs $() too:
for t in $(values ${u[tags]}); do echo $t; done   # rust shell

# filter while iterating — test each element, act on the matches:
for x in $(values $xs); do
  if [[ $x -gt 15 ]]; then echo "big: $x"; fi      # big: 20  big: 30
done

# MEMBERSHIP — RHS must be a collection (see Test Expressions):
[[ rust in $(values ${u[tags]}) ]]                 # element present?
[[ name in $u ]]                                   # record has key?
[[ 1 in $(keys $xs) ]]                             # index in bounds?

tojson $u                 # serialize back to JSON text (--pretty to indent)
```"#,
    ),
    syntax_section(
        "paths",
        "Paths",
        r#"```sh
/usr/bin/foo              # absolute
../parent/file            # relative with ..
./script.sh               # dot-slash (explicit relative)
~/src/project             # tilde expands to $HOME
cd                        # bare cd goes to $HOME
cd -                      # previous directory
```"#,
    ),
    syntax_section(
        "quoting",
        "Quoting",
        r#"```sh
"hello $NAME"             # double quotes — interpolation
"literal \$X"             # escape $ to prevent expansion
'hello $NAME'             # single quotes — literal, no interpolation

# Quote to JOIN text with interpolation — kaish does not paste adjacent
# unquoted tokens into one word (no implicit concatenation):
"$dir/file.txt"           # one path
"out-$(date +%s).log"     # one filename (text + command substitution)
echo "/tmp/$(id -u).sock" # one argument

# Unquoted text adjacent to an expansion is a PARSE ERROR (quote the word):
echo $dir/file.txt        # error — quote "$dir/file.txt"
echo /tmp/$(id -u).sock   # error — quote "/tmp/$(id -u).sock"
cmd > $dir/out.txt        # error — quote "$dir/out.txt"
# (single-token words like file.txt or v1.2.3 are fine unquoted)
```"#,
    ),
    syntax_section(
        "pipes-redirects",
        "Pipes & Redirects",
        r#"```sh
cmd1 | cmd2 | cmd3        # pipe stdout
cmd > file                # write stdout
cmd >> file               # append
cmd < file                # stdin from file
cmd 2> file               # stderr
cmd &> file               # stdout + stderr
cmd 2>&1                  # merge stderr into stdout

cat <<EOF                 # here-doc
content with $VAR
EOF

jq -r '.name' <<< "$R"    # here-string — feed expanded word to stdin

cmd > "$dir/out.log"      # quote interpolated targets — one word required
cat < "$(find-config)"    # command substitution works in a quoted target
```

A redirect target is a single word: quote it when it interpolates
(`> "$dir/f"`, not `> $dir/f`). Bare command substitution as the whole
target (`> $(cmd)`) works; bare text-plus-interpolation does not.

One stdin source per command: `<`, `<<`, and `<<<` cannot be combined.
jq is built-in (native jaq), so `<<<` + jq replaces `echo … | jq`
without a subprocess. jq also accepts real jq's `--arg NAME VALUE`,
`--argjson NAME VALUE`, and `-n` / `--null-input` flags for binding
kaish variables directly into the filter."#,
    ),
    syntax_section(
        "operators",
        "Operators",
        r#"```sh
cmd1 && cmd2              # cmd2 if cmd1 succeeds
cmd1 || cmd2              # cmd2 if cmd1 fails
```"#,
    ),
    syntax_section(
        "test-expressions",
        "Test Expressions",
        r#"```sh
# File: -f (file) -d (dir) -e (exists) -r (readable) -w (writable) -x (executable)
# String: -z (empty) -n (non-empty) == != =~ (regex) !~ (not regex)
# Numeric: -gt -lt -ge -le
# Logic: && || !
# Membership: in (list→element, record→key) / not in — RHS must be a collection

[[ -f config.json && -n $NAME ]]
[[ $N -gt 5 ]]
[[ $s =~ "\.rs$" ]]
[[ banana in $fruits ]]
[[ tmp not in $services ]]
```"#,
    ),
    syntax_section(
        "control-flow",
        "Control Flow",
        r#"```sh
if [[ -f file ]]; then echo "found"; elif [[ -d dir ]]; then echo "dir"; else echo "none"; fi

for item in "one" "two"; do echo $item; done
for f in *.txt; do cat "$f"; done
for x in $(values $list); do echo $x; done            # a collection's elements/values
for k in $(keys $rec); do echo "$k=${rec[$k]}"; done  # a record's keys (bare $rec is E012)

while [[ $N -gt 0 ]]; do N=$((N - 1)); done

case $VAR in
    hello) echo "matched" ;;
    *.rs) echo "Rust file" ;;
    *) echo "default" ;;
esac

break; continue; return [N]; exit [N]
```"#,
    ),
    syntax_section(
        "command-substitution",
        "Command Substitution",
        r#"```sh
NOW=$(date)

# In for-loops, $(cmd) splits on newlines (only):
for line in $(cat file); do echo $line; done   # per-line iteration
for x in $(echo "a b c"); do echo $x; done     # one iteration (no \n)

# Whitespace splitting needs explicit split:
for x in $(split "a b c"); do echo $x; done

# Builtins that emit .data (seq, jq, cut, find, glob) iterate per element:
for i in $(seq 1 5); do echo $i; done

# Outside for-loops, $(cmd) is one value:
R=$(printf 'a\nb')                # R is "a\nb"
echo "got: $(printf 'x\ny')"      # one echo, newline preserved

# A $(...) body takes the full statement grammar: &&/|| chains, ; sequences,
# multi-line bodies, and # comments (not just a single pipeline):
H=$(cd "$repo" && git rev-parse HEAD)
B=$(printf a; printf b)           # "ab"

# kaish-last prints the previous command's .data (or its stdout) as text:
seq 1 5
kaish-last | jq '.[2]'            # → 3
seq 1 5
DATA=$(kaish-last)                # capture for later use
```"#,
    ),
    syntax_section(
        "arithmetic",
        "Arithmetic",
        r#"```sh
X=$((5 + 3))              # 8
Y=$((X * 2))              # 16
# Operators: + - * / % > < >= <= == !=
# Comparisons return 1/0
```"#,
    ),
    syntax_section(
        "functions",
        "Functions",
        r#"```sh
greet() { echo "Hello, $1!"; }
function greet { echo "Hello, $1!"; }
greet "Amy"
```"#,
    ),
    syntax_section(
        "glob-expansion",
        "Glob Expansion",
        r#"```sh
ls *.txt                  # expands to matching .txt files
cat src/*.rs              # path-prefixed globs work
for f in *.json; do       # iterates over matches
    jq ".name" "$f"
done
set +o glob               # disable bare glob expansion
set -o glob               # re-enable (on by default)
```

Zero matches is an error (exit code 1). The `glob` builtin still works for `--exclude` and `**`."#,
    ),
    syntax_section(
        "shell-options",
        "Shell Options",
        r#"```sh
set -e                    # exit on first error
set -o latch              # require nonce confirmation to delete/overwrite (exit code 2)
set -o trash              # move rm'd / overwritten files to Trash
set -o glob               # enable bare glob expansion (on by default)
set +o latch              # disable latch
set +o trash              # disable trash
set +o glob               # disable bare glob expansion
```

Env vars: `KAISH_LATCH=1`, `KAISH_TRASH=1` enable at startup.

**Latch:** Nonces scoped to (command, paths). A nonce for `rm A` rejects `rm B`.
Confirmed paths must be subset of authorized paths. Exit code 2 = needs confirmation.
Applies to `rm` and to truncating overwrites (`tee`, `patch`, `sed -i`) —
confirm those with `--confirm=<nonce>`. `tee -a` append, new files, and
`patch --dry-run` don't gate. The prompt prints to stderr (stdout stays empty);
the nonce is on the result's `data` field — nested under `data` in the error
envelope when `--json` is on.

**Trash:** Files <= 10MB and directories always trash. `/tmp`, `/v/*` excluded.
A truncating overwrite under `trash` snapshots the file's prior content first
(recoverable from Trash). If trash fails, the op errors (no silent fallthrough to a
destructive delete/overwrite). Configure threshold: `kaish-trash config max-size <bytes>`.

**Nonce persistence:** The kernel creates a fresh nonce store by default.
An embedder can share one store across `execute()` calls in a session, so a
nonce from call 1 can confirm in call 2. The REPL keeps one kernel alive —
nonces persist naturally. Embedders control this via
`KernelConfig::with_nonce_store()`."#,
    ),
    syntax_section(
        "error-handling",
        "Error Handling",
        r#"```sh
set -e                    # exit on first error
cmd || { echo "failed"; exit 1; }
source utils.kai          # load script (shared scope)
```"#,
    ),
    syntax_section(
        "aliases-background-jobs",
        "Aliases & Background Jobs",
        r#"```sh
alias ll='ls -la'         # define (first word only, not in pipelines)
unalias ll                # remove

slow-task &               # run in background
jobs                      # list jobs
wait %1 %2                # wait for specific jobs
```"#,
    ),
];
