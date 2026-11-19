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
        "kaish (会sh) is a Bourne-like shell fused with typed JSON values. Every \
         builtin emits structured output. kaish validates the whole command before \
         running it, so it never half-runs. Builtins run in-process. External \
         commands run via `PATH`.",
    ),
    en(
        Concept::Model,
        "data-model",
        Variant::Rule,
        Depth::Summary,
        None,
        "Values include strings, numbers, booleans, lists (`xs=[a b c]`), and \
         records (`u={k: v}`). Access collections with brackets only (`${r[key]}`, \
         `${xs[0]}`). Iterate with `$(values $c)` or `$(keys $c)`. Convert JSON \
         text with `fromjson` and `tojson`. See `help syntax` → Collections.",
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
        "**No word splitting.** `$VAR` is always one argument, even when it holds \
         spaces. Use `split` to split on whitespace, a delimiter, or a regex.",
    )
    .ranked(0),
    en(
        Concept::Foundations,
        "no-word-splitting",
        Variant::Contrast,
        Depth::Reference,
        None,
        "kaish never splits unquoted `$VAR` on `$IFS`. Pass `$VAR` unquoted to \
         keep its value whole.",
    ),
    en(
        Concept::Foundations,
        "quote-to-join",
        Variant::Rule,
        Depth::Summary,
        None,
        "`\"$dir/file.txt\"` is one path. kaish keeps `$VAR`, `$(cmd)`, and globs \
         as separate words. Quote the whole word to join text with an expansion.",
    )
    .ranked(1),
    en(
        Concept::Foundations,
        "quote-to-join",
        Variant::Contrast,
        Depth::Reference,
        None,
        "Write `echo \"$dir/file.txt\"` to pass one path. Unquoted `echo \
         $dir/file.txt` is a parse error because kaish keeps each unquoted token \
         separate. Quote the whole word to fix it.",
    ),
    en(
        Concept::Foundations,
        "compound-pipeline-stage",
        Variant::Rule,
        Depth::Summary,
        None,
        "**A compound statement is a pipeline stage.** `for f in a b; do echo $f; \
         done | grep a` works in any pipeline position. Compound stages buffer all \
         output before the next stage reads it. `for … done | head -n 1` runs \
         every iteration.",
    )
    .ranked(3),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Structured output.** Every builtin emits JSON with `--json` \
         (`ls --json`, `ps --json`, `kaish-vars --json`). Success returns the \
         value unwrapped. Nonzero exits return `{\"code\":N,\"error\":\"…\"}`, \
         with partial results under `data` or `output`. Check the exit code \
         first. Spills keep their preview and metadata.",
    )
    .ranked(8),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Example,
        Depth::Reference,
        None,
        "```sh\nls -l --json | jq -r '.[].NAME'\n```",
    ),
    en(
        Concept::Foundations,
        "newline-split",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Newline-split substitution.** `for x in $(cmd)` splits on newlines \
         only. Spaces inside a line never split.",
    )
    .ranked(5),
    en(
        Concept::Foundations,
        "structured-substitution",
        Variant::Rule,
        Depth::Summary,
        None,
        "`$(cmd)` carries structured data. `for i in $(seq 1 5)` iterates five \
         values. Enumerate a collection with `for x in $(values $c)` for values \
         or `for k in $(keys $c)` for keys. Bare `for x in $c` is an error. Wrap \
         the collection in `$(values $c)` or `$(keys $c)`.",
    )
    .ranked(4),
    en(
        Concept::Foundations,
        "glob-strict",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Strict globs.** `*.txt` expands to matching files. Zero matches fail \
         with exit code 1.",
    )
    .ranked(6),
    en(
        Concept::Foundations,
        "bracket-test-not-a-command",
        Variant::Rule,
        Depth::Summary,
        None,
        "**`[ … ]` is not a command.** No `[` builtin exists, so \
         `if [ -f file ]; then` fails. Use `[[ … ]]` or `test` instead. Write \
         `if [[ -f file ]]; then …; fi` or `test -f file && echo yes`.",
    )
    .ranked(7),
    en(
        Concept::Foundations,
        "pre-validation",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Pre-validation.** kaish checks the whole command before running it. \
         Syntax errors fail up front, so a command never half-runs.",
    )
    .ranked(9),
    en(
        Concept::Foundations,
        "crash-not-corrupt",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Fail loud, not silent.** kaish returns an error instead of corrupting \
         data. Run `set -o trash` to snapshot deleted or overwritten files for \
         recovery.",
    )
    .ranked(10),
    en(
        Concept::Foundations,
        "boolean-literals-are-lowercase",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Only lowercase `true`/`false` are booleans.** `TRUE`, `yes`, `no`, \
         `on`, and `off` are strings. `x=TRUE` binds the string `\"TRUE\"`. \
         Write `true` or `false` for booleans, and check types with `typeof`.",
    )
    .ranked(11),
    en(
        Concept::Foundations,
        "json-orchestration",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Agent),
        "When orchestrating tools, pipe `--json` output through `jq`. Structured \
         data avoids scraping text.",
    )
    .ranked(12),
    en(
        Concept::Foundations,
        "collection-literals",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Collection literals.** Build lists with `xs=[a b c]` and records with \
         `u={k: v}`. `push xs v` appends in place and takes a bare variable name \
         like `read` or `unset`. Spread `[...$xs]` flattens a list, while `[$xs]` \
         nests it.",
    )
    .ranked(13),
    en(
        Concept::Foundations,
        "collection-literals",
        Variant::Contrast,
        Depth::Reference,
        None,
        "Access collections with brackets only. `${u.name}` is a parse error. \
         Write `${u[name]}` instead.",
    ),
    // ---- Overlay: opt-in, excluded from every default Recipe -----------------
    // See Concept::Overlay's doc comment for why this isn't in the Foundations
    // spine. Compose it in explicitly: `Recipe::agent_onboarding().with_overlay()`.
    en(
        Concept::Overlay,
        "overlay-mode",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Agent),
        "**Overlay mode** is opt-in via `--overlay`. Writes land in a virtual \
         memory layer until you commit. Inspect and apply changes with `kaish-vfs`:\n\
         - `kaish-vfs status`: show dirty flag and counts.\n\
         - `kaish-vfs diff`: show unified diff.\n\
         - `kaish-vfs commit`: write changes to disk.\n\
         - `kaish-vfs reset [path]`: discard edits.\n\n\
         Each fresh kernel starts a new overlay transaction. Run `kaish-vfs commit` \
         in the same call as the writes, or the transaction is lost.",
    ),
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
        "numbers",
        "Numbers",
        r#"```sh
echo 007        # the string 007 (leading zero is not a JSON number)
chmod 0644 f    # the string 0644 (mode keeps every digit you typed)
echo -0         # the number 0 (valid JSON; -0 and 0 are the same number)
echo "-0"       # the string -0 (quote to keep those two characters)
break 7         # loop count is a number
break 007       # error — leading zero is a string, write 7
```

`007`, `010`, and `00` are strings. `-0`, `0.10`, and `1.0` are numbers. Bare words keep their typed spelling in command arguments. Through a variable, arithmetic, or `--json`, they normalize to plain numbers: `x=-0; echo $x` prints `0`. Quote a number to force a string.

Leading zeros error where kaish expects a number. This covers `break` and `continue` counts, arithmetic, numeric tests, and list indices. kaish does not read leading-zero octal, so `$((010 + 1))` errors and names `8#10` or `10`. Record keys are text, so `${r[007]}` reads key `"007"`.

Bare integers must fit in signed 64 bits (`-9223372036854775808` to `9223372036854775807`). Longer numerals error and name the limit. Quote them to keep text."#,
    ),
    syntax_section(
        "expansion",
        "Expansion",
        r#"```sh
$VAR                      # simple
${VAR}                    # braced
${VAR:-default}           # default if unset/empty
${#VAR}                   # string length, in characters
$0 $1 $@ $#              # script name, args, all args, count
$?                        # last exit code (0-255)
$$ ${$}                   # kaish session id
```

`$$` expands to the kaish session ID, not the OS PID. Session IDs start at 1 and increment per session. Subshells inherit the parent session ID."#,
    ),
    syntax_section(
        "collections",
        "Collections (lists & records)",
        r#"```sh
# CONSTRUCTION — native literal syntax, commas optional.
xs=[apple banana cherry]  # list — space-separated, like shell words
nums=[1 2 3]               # ≡ [1, 2, 3]
empty=[]                   # empty list
u={name: amy, role: maintainer}   # record — bareword keys
compact={port:8080}        # colon may be spaced or unspaced
r={"content-type": x}      # quoted key for non-barewords
d={"$k": x}                # double-quoted keys interpolate; '$k' stays literal
nested={tags: [a b], meta: {active: true}}   # nested list and record

# SPREAD (...$xs) flattens; bare $var nests as one element:
xs=[1 2]
ys=[0 $xs 4]                # [0,[1,2],4]  — nests
new=[...$xs date]           # [1,2,"date"] — flattens

# JSON BRIDGE — fromjson and tojson convert JSON text:
u=$(fromjson '{"name":"amy","tags":["rust","shell"]}')
xs=$(fromjson '[10,20,30]')

# PASSING TO COMMANDS/FUNCTIONS — literals are assignment-only.
# Arguments pass as JSON text; restore typed values inside a function with fromjson:
f() { local d=$(fromjson "$1"); typeof $d; }
cfg={a: 1}; f $cfg        # record ($cfg passes as JSON text; fromjson restores it)
f [x y]                   # error — bind xs=[x y] first; literals are assignment-only

# READ ACCESS — brackets only, never dots.
${u[name]}                # record key (bareword = literal key)
${u["a key"]}             # quoted key for spaces or symbols
${u[$k]}                  # dynamic key ($var)
${xs[0]}   ${xs[-1]}      # list index; negative counts from the end
${xs[0:2]}                # slice (end-exclusive) → list
${s[0:5]}   ${s[-3:]}     # string slice → characters, not bytes
${u[tags][0]}             # nested path
${#xs}   ${#u}            # length: list elements or record keys
${u.name}                 # error — brackets only, use ${u[name]}
${s:0:5}                  # error — slice with brackets, use ${s[0:5]}

# ENUMERATE — wrap the collection in $(keys …) or $(values …).
# keys → indices (list) or keys (record). values → elements (list) or values (record).
for x in $(values $xs); do echo $x; done          # list elements: 10 20 30
for i in $(keys $xs);   do echo $i; done          # list indices: 0 1 2
for k in $(keys $u);    do echo $k; done          # record keys: name tags
for v in $(values $u);  do echo $v; done          # record values
for x in $xs;           do echo $x; done          # error — wrap $xs in $(values $xs)

# Key and value together:
for k in $(keys $u); do echo "$k = ${u[$k]}"; done

# Nested collection — subscript access needs $() too:
for t in $(values ${u[tags]}); do echo $t; done   # rust shell

# Filter while iterating:
for x in $(values $xs); do
  if [[ $x -gt 15 ]]; then echo "big: $x"; fi      # big: 20  big: 30
done

# MEMBERSHIP — RHS must be a collection (see Test Expressions):
if [[ rust in $(values ${u[tags]}) ]]; then echo "has it"; fi   # element present?
if [[ name in $u ]]; then echo "has it"; fi                     # record has key?
if [[ 1 in $(keys $xs) ]]; then echo "in bounds"; fi            # index in bounds?

# SHAPE GUARD — test a variable's type before iterating:
if [[ -record $data ]]; then
  for k in $(keys $data); do echo $k; done
elif [[ -list $data ]]; then
  for x in $(values $data); do echo $x; done
fi

tojson $u                 # serialize to JSON text (--pretty to indent)
# Use fromjsonl and tojsonl for JSONL/NDJSON streams.

# ASSIGNMENT — write via bracket paths in place (no spaces around =).
# Intermediates must exist; path assignment only creates the final record key.
xs[0]=9                   # in-bounds index update (negative index works too)
u[host]=localhost         # record key: insert or update
s[web][port]=9000         # deep path
xs[9]=x                   # error — index out of bounds (push grows lists)
s[api][port]=1            # error — missing `api` key (initialize intermediate first)
user.email=x              # error — brackets only, use user[email]=x

# push appends to a list in place. Pass a bare variable name or bracket path like read/unset.
push xs date               # xs is now […,"date"]
push xs $rec               # values push as typed values, not stringified text
push services[web][tags] canary   # bracket-path target
```"#,
    ),
    syntax_section(
        "paths",
        "Paths",
        r#"```sh
/usr/bin/foo              # absolute
../parent/file            # relative with ..
.git/HEAD                 # dot-prefixed directory
2026/report               # numeric directory name
./                        # current directory
../                       # parent directory
./script.sh               # dot-slash (explicit relative)
~/src/project             # tilde expands to $HOME
cd                        # bare cd goes to $HOME
cd -                      # previous directory

# Tilde expands only at the start of an unquoted word:
echo ~/x                  # /home/amy/x
echo '~/x'                # ~/x (quoted tilde stays literal)
```"#,
    ),
    syntax_section(
        "quoting",
        "Quoting",
        r#"```sh
"hello $NAME"             # double quotes — interpolation
"literal \$X"             # escape $ to prevent expansion
'hello $NAME'             # single quotes — literal, no interpolation
printf '<%s>\n' a\ b      # backslash quotes the next character: one argument
printf '<%s>\n' \( \)     # escaped parentheses are arguments
find . -name \*.rs        # find receives the pattern *.rs
# Mixed escapes and unquoted globs error; quote the whole literal word.
# Backslash does not join text to an unquoted expansion.

# Quote the whole word to join text with interpolation:
echo "$dir/file.txt"      # one path
echo $dir/file.txt        # error — quote the whole path
echo "/tmp/$(id -u).sock" # one argument
echo /tmp/$(id -u).sock   # error — quote "/tmp/$(id -u).sock"
cmd > "$dir/out.txt"      # one redirect target
cmd > $dir/out.txt        # error — quote "$dir/out.txt"
"out-$(date +%s).log"     # one filename (text + command substitution)

# Literal words keep their complete spelling without quotes:
echo HEAD^2 HEAD~1 f.txt~ 9-  # ^, mid-word ~, and 9- stay literal
printf 'abcdefghijkl\n' | cut -c 9-  # open range stays one word
echo 123.txt 1.2.3 true:foo a+b .git/HEAD
p=~/x; echo "$p"          # assignment followed by a home-relative path
ls 1.0*                  # glob keeps the written numeric prefix
```"#,
    ),
    syntax_section(
        "comments",
        "Comments",
        r#"```sh
# whole-line comment
echo hi                   # trailing comment (space before # required)

# `#` starts a comment only at the start of a word. Mid-word `#` is literal:
echo abc#3                # prints abc#3
echo 2d25fb02#3           # prints 2d25fb02#3 (id keeps its #<seq>)
echo https://ex.com/p#sec # prints whole URL, fragment included
echo abc #3               # prints abc (space makes #3 a comment)

# `#` after a word-boundary token errors instead of dropping the rest of the line:
echo "$x#3"               # valid — quote the whole word
echo $x#3                 # error — # after a variable reference
echo "$(echo a)#3"        # valid
echo $(echo a)#3          # error — # after a closing )

# Put a space before `#` after a closing ) ] }:
case $x in a) # comment   # valid
case $x in a)# comment    # error — add a space before #
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

cmd > "$dir/out.log"      # quote interpolated targets (one word required)
cat < "$(find-config)"    # command substitution works in a quoted target
```

A redirect target is one word. Quote targets that combine text and interpolation (`> "$dir/f"`). Bare command substitution (`> $(cmd)`) works as a whole target.

```sh
{ echo first; echo second; } > log
while read line; do echo "$line"; done < input
```

Redirects after a group, `if`, `for`, `while`, or `case` apply to the whole body. Targets open before the body runs, and input returns to the session afterward. An empty redirected compound exits 2. Use `: > log` to create an empty file. Pipe here-docs into compound statements:

```sh
cat <<EOF | while read line; do echo "$line"; done
one
EOF
```

Pass one stdin source per command (`<`, `<<`, or `<<<`). A redirect applies only to its command, so `seq 3 | jq . < f` leaves session stdin intact. `jq` is a builtin, so `<<<` feeds `jq` without a subprocess. Pass `--arg NAME VALUE`, `--argjson NAME VALUE`, and `-n` / `--null-input` to bind variables into filters.

```sh
printf 'a\nb\n' | jq -R .    # one JSON string per line
printf 'a\nb\n' | jq -R -s . # the whole text as one string
```

`jq -R` reads raw text instead of JSON. Failing lines print to stderr while remaining lines run, and the command exits 1. `-n -R` exits 2. Use `jq -R .` or `jq -R -s .` instead."#,
    ),
    syntax_section(
        "operators",
        "Operators",
        r#"```sh
cmd1 && cmd2              # cmd2 if cmd1 succeeds
cmd1 || cmd2              # cmd2 if cmd1 fails

if ! cmd; then …; fi      # `!` negates a condition
while ! cmd; do …; done   # negates loop condition (`until` spelling)

! true                    # `!` also negates a pipeline as its own statement
! a | b                   # binds to the whole pipeline, not just `a`
!true                     # error — add a space: `! true`
f() { ! cmd; }; f &       # negate inside a function, then background the call
! cmd &                   # error — wrap negation in a function to background it
```

`!` binds only to the pipeline that follows, not the whole chain. `! a && b` parses as `(! a) && b`, so `! true && echo x` never prints `x`. A negated statement never trips `set -e`. See `help syntax` → Shell Options for the `! grep -q` hazard."#,
    ),
    syntax_section(
        "test-expressions",
        "Test Expressions",
        r#"```sh
# File: -f (file) -d (dir) -e (exists) -r (readable) -w (writable) -x (executable)
#   -L (symlink, alias -h) — matches dangling symlinks too, unlike -e
# String: -z (empty) -n (non-empty) == != =~ (regex) !~ (not regex)
# Type: -list -record — false for other types; errors on unset $var (use with typeof)
# Numeric: -gt -lt -ge -le
# Logic: && || !
# Membership: in (list element or record key) / not in — RHS must be a collection

if [[ -f config.json && -n $NAME ]]; then echo "ready"; fi
if [[ -L /path/link ]]; then echo "symlink"; fi
if [[ $STATUS != "ok" ]]; then echo "failed"; fi
if [[ $N -gt 5 ]]; then echo "big"; fi
if [[ $s =~ "\.rs$" ]]; then echo "rust"; fi
if [[ banana in $fruits ]]; then echo "have one"; fi
if [[ tmp not in $services ]]; then echo "not running"; fi
if [[ -list $x ]]; then echo "list"; fi
if [[ -record $x ]]; then echo "record"; fi
```"#,
    ),
    syntax_section(
        "control-flow",
        "Control Flow",
        r#"```sh
if [[ -f file ]]; then echo "found"; elif [[ -d dir ]]; then echo "dir"; else echo "none"; fi

for item in "one" "two"; do echo $item; done
for f in *.txt; do cat "$f"; done
for x in $(values $list); do echo $x; done            # collection elements/values
for k in $(keys $rec); do echo "$k=${rec[$k]}"; done  # record keys (bare $rec errors)

while [[ $N -gt 0 ]]; do N=$((N - 1)); done

case $VAR in
    hello) echo "matched" ;;
    *.rs) echo "Rust file" ;;
    *) echo "default" ;;
esac

break; continue; return [N]; exit [N]

{ echo first; echo second; }       # group in the current shell
false || { echo failed; exit 1; }  # fallback with several statements

# A group keeps variable, cwd, and function changes. Its last statement
# supplies its status; control flow leaves the group for the enclosing body.
# With set -e, `{ false && true; }` lets the next statement run.

# A compound statement is a pipeline stage in any position. It buffers:
# the whole block runs before the next stage reads a byte.
for f in a b; do echo $f; done | grep a
printf "a\nb\n" | while read l; do echo "got $l"; done

# `:` is the null command (exits 0, like `true`):
if [[ -f log.txt ]]; then :; else touch log.txt; fi   # empty branch
: > log.txt                                           # truncate to zero bytes
```"#,
    ),
    syntax_section(
        "command-substitution",
        "Command Substitution",
        r#"```sh
NOW=$(date)

# In for-loops, $(cmd) splits on newlines only:
for line in $(cat file); do echo $line; done   # per-line iteration
for x in $(echo "a b c"); do echo $x; done     # one iteration (no \n)

# Quote the whole word to join text with interpolation:
for x in "$(echo foo)/b"; do echo "$x"; done  # one item: foo/b
# Separate loop items with whitespace; adjacent fragments error.

# Whitespace splitting needs explicit split:
for x in $(split "a b c"); do echo $x; done

# A line filter returns text, so newline splitting iterates it:
for i in $(seq 1 5); do echo $i; done

# Value-returning builtins (fromjson, jq, keys, values, split, gather) bind
# their value directly. Pass --json to other builtins:
cfg=$(fromjson '{"port": 80}');  echo ${cfg[port]}
y=$(cut -f2 f)                    # text printed by cut
y=$(cut -f2 f --json)             # ["b"]

# Outside for-loops, $(cmd) is one value:
R=$(printf 'a\nb')                # R is "a\nb"
echo "got: $(printf 'x\ny')"      # one echo, newline preserved

# A $(…) body accepts the full statement grammar (&&/||, ;, multi-line bodies,
# comments, and control structures), quoted or unquoted:
H=$(cd "$repo" && git rev-parse HEAD)
B=$(printf a; printf b)           # "ab"
L=$(for f in *.txt; do wc -l < "$f"; done)

# An assignment with no command returns the last $(…) exit code, or 0.
# A failed substitution triggers || fallbacks and set -e:
x="$(read-config)" || x="default"   # fallback runs when read-config fails

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
echo $((5 + 3 * 2))       # 11    checked 64-bit integers; overflow errors
echo $((0xff))            # 255   hex; also $((16#ff)) (base#digits, base 2 to 36)
echo $((8#17))            # 15    octal; $((010)) errors — write 8#10 or 10
m=$(date +%m); echo $((10#$m + 1))   # text with a leading zero, read as decimal
echo $((2 ** 10))         # 1024
echo $((5 > 3))           # 1     comparisons return 1 or 0
echo $((a > b ? a : b))   # ternary; && || ? : skip the unused branch
echo $(( $(wc -l < f) * 2 ))         # $(cmd) is an operand
x=$((x + 1))              # assignment stays outside
(( x++ ))                 # error — assign outside with x=$((x + 1))
while (( i <= 5 )); do …; done     # (( )) alone is a condition
```

Operators, highest first: `+ - ! ~` · `**` · `* / %` · `+ -` · `<< >>` · `< <= > >=` · `== !=` · `&` · `^` · `|` · `&&` · `||` · `? :`.
An unset variable, a float, or `$RANDOM` errors and names the fix."#,
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

Zero matches exit 1. The `glob` builtin takes unquoted patterns (`glob **/*.rs`), multiple patterns, `--exclude`, `--ftype`, and depth flags.

```sh
grep -rn TODO src          # skip discovered symlinks, devices, FIFOs and sockets
grep -R -l TODO src        # read file symlinks; do not enter linked directories
```

`grep` reads explicitly named paths. Walk and read errors print the path to stderr, keep other matches, and exit 2. `-q` exits 0 on a match even after an error. Recursive and multi-file searches read 256 KiB chunks. `-q` and `-l` stop at the first match per file. `-m N` stops after `N` matched lines, and `-m 0` reads no contents. Context output, multiline matching, and encoding conversion buffer whole files."#,
    ),
    syntax_section(
        "builtin-flags",
        "Builtin Flags",
        r#"```sh
find . -type f '(' -name '*.rs' -o -name '*.md' ')'
find . ! -name '*.log' -print
cat -A file                # show nonprinting bytes, tabs, and line ends
ls -d directory            # list the directory itself; overrides -R
grep -x 'ready' file       # match a whole line; overrides -w
find --help                # print help and exit 0
```

In `find`, `!` binds before `-a` (implied between tests), then `-o`. Quote parentheses to pass them to `find`. Tests short-circuit, and each reached `-print` prints once. Without `-print`, matching entries print once. Expressions allow at most 256 nodes and 64 nested groups or negations. Larger expressions exit 2 and name the limit.

`--help` and `--json` after an option that takes a value stay data: `find . -name --help` searches for `--help`. Unsupported builtin flags exit 2 and point to `help`."#,
    ),
    syntax_section(
        "regex",
        "Regex (grep, sed, awk)",
        r#"```sh
# grep and sed (without -E/-r) read GNU BRE: bare ( ) { } | + ? are literal
grep -n 'fn consult(' src/lib.rs    # literal paren
sed -n '/fn consult(/p' src/lib.rs  # same rule in sed address
grep 'error\|warn' log.txt          # \| alternation; \(…\) group; \{2,5\} interval; \+ \?
sed 's/error\|warn/X/'              # sed reads the same escapes as operators
grep -E 'error|warn' log.txt        # -E: ERE, bare a|b (…) x+ y? z{2,5}
grep -E -o '\d' f                   # -E reads the same escapes: \d is literal d
sed -E 's/v([0-9]+)\.[0-9]+/\1/'    # -E/-r: ERE; \1 \2 work in the replacement either way
grep -F 'a|b' f                     # -F: fixed string, no metacharacters
grep '[0-9]\+\.\w\b' f              # \w \s \b \< \> work; \d is literal d

# awk always uses ERE: bare ( ) { } | + ? are operators,
# and \( \) \{n,m\} \| \+ \? are literal
awk '/^(GET|POST) /' access.log     # bare (…) and | are ERE operators
awk '/fn consult\(/ {print}'        # \( is a literal paren
awk '{gsub(/a\|b/, "X")}'           # \| is a literal pipe, not alternation

sed 's/[|]/,/g' f                   # literal | anywhere: bracket class, or \| in sed BRE
sed 's/\(a\)\1/x/'                  # error — back-references not supported in patterns (use \1 in replacement only)
```"#,
    ),
    syntax_section(
        "shell-options",
        "Shell Options",
        r#"```sh
set -e                    # exit on first error
set -o pipefail           # pipeline reports its rightmost failing stage
set -o trash              # move removed or overwritten files to Trash
set -o glob               # enable bare glob expansion (on by default)
set -o crossmounts        # recursive walks descend into other mounts
set +o trash              # disable trash
set +o glob               # disable bare glob expansion
set -o                    # report every option and its state
```

Recursive walks (`grep -r`, `find`, `ls -R`, `tree`, `glob`, `**`) stay in their starting mount. `grep -r x /` lists `/v` and `/dev` without searching them. Pass a mount path (`grep -r x /v`) to walk that mount and mounts nested inside it. Pass `--cross-mounts` or set `set -o crossmounts` to cross every mount. When a walk skips mounts, it names them once on stderr and keeps its exit code.

Without `pipefail`, `cat missing | wc -l` exits **0** from the last stage. With `set -o pipefail`, it exits **1**. `PIPESTATUS` is a list holding every stage's exit code. After `false | true`, `${PIPESTATUS[0]}` is `1` and `${PIPESTATUS[1]}` is `0`. `${#PIPESTATUS}` counts stages and `$(values $PIPESTATUS)` iterates them. Copy `PIPESTATUS` before running another command, because each command overwrites it.

```sh
set -e
! true                     # flips exit code to 1, does not abort
echo reached               # still runs
```

`!` negates a pipeline's exit code and never triggers `set -e`. `! grep -q secret f` exits 1 when `secret` matches, yet `set -e` ignores it. Write `grep -q secret f && exit 1` to fail on a match.

Set `KAISH_TRASH=1` to enable trash at startup.

Unknown names in `set -o NAME` or `set +o NAME` exit **2** and list valid options (`crossmounts`, `glob`, `output-limit[=SIZE]`, `pipefail`, `trash`). kaish ignores unknown short flags such as `-u` or `-x`.

**Trash:** With `set -o trash`, `rm` and overwrites (`cp`, `dd`, `edit`, `mv`, `patch`, `sed -i`, `tee`, `write`) snapshot prior content. `tee -a`, new files, and `patch --dry-run` have no prior content to snapshot. Files <= 10 MB and directories move to Trash, excluding `/tmp` and `/v/*`. `rm` unlinks symlinks directly without trashing their targets. If trashing fails, the command errors without deleting or overwriting. Set the size limit with `kaish-trash config max-size <bytes>`.

`kaish-trash empty` refuses and names `--confirm`. Run `kaish-trash empty --confirm` to discard all Trash contents. `--confirm` is a plain boolean flag, and no session option disables it."#,
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
