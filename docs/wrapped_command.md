# Wrapped commands

A wrapped command is an external program registered as a kaish tool. The
embedder declares the program, the verbs it allows, and the flags each verb
accepts. The kernel validates a call against that declaration, renders the
argv itself, and runs the program with `execve(2)` — never through `sh -c`.

Status: design, 2026-08-22. Implementation tracks this document; when the two
disagree, fix one and say which.

## What it is for

`allow_external_commands` is a single switch. Off, nothing spawns. On, every
program on `$PATH` spawns, with any arguments, and the validator sees each call
as an opaque word list. There is no setting between those two.

A wrapped command is the setting between. It is an allowlist with a grammar:

- The executable is pinned at registration. A script that changes `$PATH`
  changes nothing.
- Verbs and flags are deny-by-default. An undeclared verb or flag fails
  validation before anything spawns, and the error names the allowed set.
- The kernel renders the argv from the declaration. A value is never parsed as
  a flag by the child unless the declaration put it in flag position.
- The child's environment is the kernel's hermetic environment plus the
  declaration's pins. No `EDITOR`, no pager, no credential prompt.

`allow_external_commands = false` plus a few wrapped commands is a
mostly-hermetic kernel: every program it can run is named, and every argument
shape it can pass is declared.

This is not a reimplementation of the program. kaish-extras ships an
in-process `git` built on gitoxide, with structured output and no subprocess.
A wrapped `git` trusts the host's `/usr/bin/git` and gates it. Both stand. For
`cargo`, `go`, `make`, and `python` there is no in-process option, and the
wrapper is the only gate.

## The declaration

Five nouns and two enums:

| Noun | Meaning |
|---|---|
| `WrappedCommand` | One executable: pinned path, fixed lead argv, env pins, verbs. |
| `Verb` | A subcommand, or the root for a verb-less program like `python`. |
| `Flag` | A switch or a value flag, with its render style. |
| `Positional` | One or many positional arguments, with optional constraints. |
| `Stdin` | `Closed` (default) or `Pipe`. Per verb. |

| Enum | Variants |
|---|---|
| `Tail` | `Deny` (default), `AfterDashDash`, `Forward`. What happens to argv the declaration does not describe. |
| `Stdin` | `Closed` (default), `Pipe`. |

A declaration is data. The builder derives `serde::{Serialize, Deserialize}`,
so an embedder that wants a TOML or JSON file gets one with `toml::from_str`.
The kernel owns no config-file parser. A declaration holds no closures.

### git: allowlisted verbs

```rust
use kaish_kernel::tools::wrapped::{Flag, Positional, Tail, Verb, WrappedCommand};

let git = WrappedCommand::new("git")
    .executable("/usr/bin/git")
    .about("Version control, read-mostly. Push needs an explicit remote and refspec.")
    .lead(["--no-pager"])
    .env("GIT_PAGER", "cat")
    .env("GIT_TERMINAL_PROMPT", "0")
    .verb(Verb::new("status")
        .flag(Flag::switch("short").alias("-s"))
        .flag(Flag::value("untracked-files").choices(["no", "normal", "all"]))
        .positional(Positional::many("pathspec")))
    .verb(Verb::new("log")
        .flag(Flag::value("max-count").alias("-n").int())
        .flag(Flag::switch("oneline"))
        .flag(Flag::value("since"))
        .positional(Positional::many("revision")))
    .verb(Verb::new("diff")
        .flag(Flag::switch("stat"))
        .flag(Flag::switch("cached"))
        .positional(Positional::many("pathspec")))
    .verb(Verb::new("commit")
        .flag(Flag::value("message").alias("-m").repeatable().required())
        .positional(Positional::many("pathspec")))
    .verb(Verb::new("push")
        .flag(Flag::switch("force-with-lease"))
        .positional(Positional::one("remote").required())
        .positional(Positional::one("refspec").required()))
    .build()?;

tools.register(git);
```

What the agent sees:

```
$ git comit -m "x"
git: unknown verb 'comit'. Allowed: commit, diff, log, push, status

$ git log --output=/tmp/x
git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since

$ git push --force origin main
git: unknown flag '--force' for 'git push'. Allowed: --force-with-lease

$ branch="--output=/etc/cron.d/x"
$ git log -n 5 "$branch"
git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since

$ git log -n 5 -- "$branch"
  argv: ["/usr/bin/git", "--no-pager", "log", "--max-count=5", "--", "--output=/etc/cron.d/x"]
  (git reports no such path; the child never parses the value as a flag)
```

The first four are validation failures, exit 2. They fire from `kaish --plan`
and from `plan_program` before anything spawns. `help git` renders from the
same declaration.

### python: one mechanism, two postures

Python has no verbs, so the root is the verb. `-c`, `-m`, and `-` (read code
from stdin) are each `sh -c` in a costume. Deny-by-default means they do not
exist until a declaration names them, and naming them is the audit trail.

Locked — run the scripts this deployment ships:

```rust
let python = WrappedCommand::new("python")
    .executable("/usr/bin/python3")
    .lead(["-I"])
    .env("PYTHONDONTWRITEBYTECODE", "1")
    .root(Verb::root()
        .positional(Positional::one("script").required().path_under("/opt/app/scripts"))
        .positional(Positional::many("args"))
        .stdin(Stdin::Pipe))
    .build()?;
```

```
$ python -c 'import os; os.system("id")'
python: unknown flag '-c'. Allowed: (none)

$ python /etc/passwd
python: 'script' must be under /opt/app/scripts

$ python etl.py --stage=1 < input.csv
python: unknown flag '--stage' for 'python'. Allowed: (none)

$ python etl.py -- --stage=1 < input.csv
  argv: ["/usr/bin/python3", "-I", "/opt/app/scripts/etl.py", "--", "--stage=1"]
```

`-I` is isolated mode: the child ignores `PYTHON*` environment variables and
the user site directory. The pin and the lead together close the
`PYTHONPATH`/`sitecustomize` route into the interpreter.

Interpreter — deliberately open, with the module list as the allowlist:

```rust
let python = WrappedCommand::new("python")
    .executable("/usr/bin/python3")
    .lead(["-I"])
    .root(Verb::root()
        .flag(Flag::value("m").choices(["json.tool", "pytest", "http.server", "venv"]))
        .flag(Flag::value("c").about("Run inline code. This deployment allows it."))
        .positional(Positional::one("script"))
        .positional(Positional::many("args"))
        .stdin(Stdin::Pipe))
    .verb(Verb::new("json-tool")
        .lead(["-m", "json.tool"])
        .omit_name()
        .positional(Positional::one("file"))
        .stdin(Stdin::Pipe)
        .json_output())
    .build()?;
```

`json-tool` is a verb python does not have. `Verb::lead` supplies the real
argv and `Verb::omit_name` keeps the synthetic name out of it, so the
declaration can name a stable, narrow entry point for a module invocation:
`python json-tool f.json` renders `["-I", "-m", "json.tool", "f.json"]`. A
verb the program does have keeps its name — `cargo metadata` renders
`["metadata", "--format-version", "1"]` — so the two cases are told apart by
the declaration, not by whether a `lead` is present. `.json_output()` declares that the verb's stdout is JSON;
`$(python json-tool f.json)` binds as data because the declaration said so,
never because the bytes looked like JSON.

### cargo: the override, scoped to a verb

```rust
let cargo = WrappedCommand::new("cargo")
    .executable("/home/amy/.cargo/bin/cargo")
    .env("CARGO_TERM_COLOR", "never")
    .env("CARGO_NET_OFFLINE", "true")
    .verb(Verb::new("build")
        .flag(Flag::switch("release"))
        .flag(Flag::value("package").alias("-p")))
    .verb(Verb::new("test")
        .flag(Flag::value("package").alias("-p"))
        .positional(Positional::one("testname"))
        .tail(Tail::AfterDashDash))
    .verb(Verb::new("clippy").tail(Tail::Forward))
    .verb(Verb::new("metadata")
        .lead(["--format-version", "1"])
        .json_output())
    .build()?;
```

```
$ cargo test parser -- --nocapture
  argv: [".../cargo", "test", "parser", "--", "--nocapture"]

$ cargo clippy --all-targets -- -D warnings
  argv: [".../cargo", "clippy", "--all-targets", "--", "-D", "warnings"]
```

`Tail::Forward` on `clippy` is the override: the verb is allowed, its flags are
not modeled, and undeclared flags pass through unchanged. `help cargo` marks
the verb: `clippy — forwards undeclared flags`. A value that expands to a flag
reaches the child under `Forward`; that is the override's cost, and the
declaration shows where it was paid.

### The same declaration as data

```toml
[command.python]
executable = "/usr/bin/python3"
lead = ["-I"]
env = { PYTHONDONTWRITEBYTECODE = "1" }

[command.python.root]
tail = "after-dash-dash"
stdin = "pipe"
positional = [
  { name = "script", required = true, path_under = "/opt/app/scripts" },
  { name = "args", many = true },
]
```

## Parsing

The kernel hands a wrapped command its argv in source order, with `--`
preserved (`ToolSchema::raw_argv`). The wrapper parses that list against the
declaration. There is no clap layer.

Rules, in order:

1. The first word selects the verb. A `WrappedCommand` with a `root` verb and
   no named verbs skips this step. A word that names no verb fails: `unknown
   verb 'X'. Allowed: …`. Verb names match exactly; no prefix matching.
2. Before `--`, a word that starts with `-` is a flag. It must match a declared
   name or alias exactly. `--flag=value` and `--flag value` both bind a value
   flag; `-f value` binds a short alias. Clustered shorts (`-sv`) and glued
   short values (`-n5`) are not accepted; the error names the separated form.
   Prefix abbreviations (`--forc` for `--force`) are not accepted.
3. Before `--`, a word that does not start with `-` fills the next declared
   positional slot. A `many` positional absorbs the rest.
4. `--` ends flag parsing. After it, every word is positional. Words that fill
   no declared slot are the tail.
5. The tail goes where `Tail` says. `Deny` fails: `unexpected argument 'X'`.
   `AfterDashDash` forwards the tail after a rendered `--`. `Forward` is the
   same, and additionally forwards undeclared flags from step 2 in place.
6. A `required` flag or positional that is absent fails. A `choices` flag whose
   value is not in the set fails and names the set. An `int` flag that does not
   parse as an integer fails.

A value that came from a variable is parsed like a literal. `git log "$x"`
with `x = "--output=f"` is an unknown flag, not a positional. To pass a value
that starts with `-` as a positional, write `--` before it, as in `sh`.

Every parse failure exits 2 and names the verb, the offending word, and the
allowed set.

## Rendering

```
argv = executable
     , command.lead…
     , verb.name            (omitted for the root verb)
     , verb.lead…
     , flags…               (in declaration order)
     , positionals…         (in source order; a `--` the agent wrote stays where it was)
     , "--", tail…          (only when the tail is non-empty)
```

A flag renders under its declared name, whichever alias the agent wrote:
`-n 5` renders as `--max-count=5`. A long value flag renders as
`--name=value`. A one-character name (`Flag::value("m")`) renders as
`-m value`. A switch renders as its name. `Flag::style(Style::Separate)` or
`Flag::style(Style::Equals)` overrides the default for a flag whose program
accepts only one form. A repeatable flag renders once per occurrence, in
source order.

The wrapper never inserts a `--` the agent did not write, except to introduce
a non-empty tail. `git log -- main` means "paths named main", not "revision
main"; inserting `--` would change the program's meaning. Mirroring the
agent's `--` keeps the `sh` rule: what you wrote is what the child sees.

## Constraints

The constraint vocabulary in v1:

| Constraint | On | Check |
|---|---|---|
| `required()` | flag, positional | Present. |
| `int()` | flag | Parses as `i64`. |
| `choices([…])` | flag | Value is in the set. |
| `path_under(root)` | positional | The resolved real path is inside `root`. |

`path_under` resolves the value against the kernel's real cwd
(`resolve_real_path`), canonicalizes it, and checks that the result is inside
`root` by path component, not by string prefix: `/opt/app/scripts-evil/x` is
not under `/opt/app/scripts`. A symlink whose target leaves `root` fails. A
virtual cwd (overlay or memory mount) fails with the virtual-cwd error the
spawn path already uses. The check runs at execution, where the cwd is known.
Validation rejects an absolute path outside `root` early; a relative path is
checked when the call runs.

The vocabulary grows by one rule: a constraint earns a field when it needs the
kernel's resolution (paths, cwd, mounts), or when a real declaration needed it.
Not in advance.

## Execution

1. Parse and constrain (above). Failure exits 2; nothing spawns.
2. Environment: the kernel's hermetic child environment (exported scope
   variables only, never the kaish process's OS environment), then the
   declaration's `env` pins, which win on conflict.
3. cwd: the kernel's real cwd. A virtual cwd fails as it does for external
   commands.
4. stdin: `Closed` gives the child `/dev/null`. If the call has piped or
   redirected stdin and the verb is `Closed`, the call fails, exit 2: `<name>
   <verb>: does not read stdin`. Silent drop is not an option. `Pipe` streams
   the kernel's stdin to the child with the same discipline as an external
   command (no draining before spawn; a pipeline stays a pipeline).
5. stdout and stderr are always captured. A wrapped command never inherits
   the terminal; `exec` and external commands cover interactive use.
6. Cancellation and timeouts use the same `wait_or_kill` discipline as an
   external command: the cancel token from `ExecContext`, the kernel's kill
   grace, process-group signaling, `kill_on_drop`.
7. Output limits and spill apply as for an external command.
8. The child's exit code is the result's exit code, unchanged. The program's
   own code is the contract; the wrapper adds nothing on top.
9. A `json_output()` verb parses stdout as JSON and returns it as the result's
   data, so `$(…)` binds it typed. Stdout that is not valid JSON is an error,
   exit 1, naming the verb and the parse failure — not a silent fall back to
   text.

## Registration

```rust
let kernel = Kernel::with_backend(backend, config, |_| {}, |tools| {
    tools.register(git);
    tools.register(python);
})?;
```

`build()` verifies the executable: an absolute path that exists, is a file,
and has an execute bit. Anything else is an `Err` at registration, not a
`127` on first call. `wrapped::find_executable(name, path_var)` resolves a bare
name against a `PATH` string the embedder supplies; the kernel does not read
the OS environment to find one.

A wrapped command is a registered tool, not an external command. It runs when
`allow_external_commands` is `false`. It needs the `subprocess` feature; a
sandbox build has no `wrapped` module.

## What the kernel owns, and what the embedder owns

The embedder holds the policy: which programs, which verbs, which flags, which
directory. The kernel holds the mechanism that makes the policy correct: the
parse, the render, the real-path resolution, the hermetic spawn, the cancel
discipline, the output limits.

`path_under` is the case to test the line against. An embedder could check the
path itself, from a hook. The kernel does it instead, from a declared value,
because:

- A declared value fires at validation. A hook fires at execution, after a
  plan has already said the call was fine.
- A declared value publishes. `help python` says `script — must be under
  /opt/app/scripts`; the error names the fix. A hook is opaque to both.
- The path the child sees is the one the kernel rendered, resolved against the
  kernel's cwd, through the kernel's symlink rules. A check outside the kernel
  checks a different string.

The kernel still decides nothing about *which* directory is safe. That value
is the embedder's, in the declaration, the same way `choices` is.

Richer policy composes above the boundary. An embedder with an approval flow
reads the rendered argv from the plan and decides on its own time; the wrapper
validated the declared shape first. Neither replaces the other.

## Hazards

| Hazard | Fix kaish ships |
|---|---|
| A value expands to text that starts with `-`. | Parsed as a flag; undeclared flags fail. Write `--` to pass it as a positional. |
| `Tail::Forward` forwards an expanded flag. | None. The override's cost; `help` marks the verb. |
| The program reads stdin and would hang. | `Stdin::Closed` by default gives `/dev/null`. |
| The program opens `$EDITOR` or a pager. | Hermetic env has neither; pin `GIT_PAGER=cat` where the program defaults to one. |
| The declared flags drift from the installed program. | The child's own usage error, at the child's exit code. Pin `executable` to a known install. |
| A value contains a NUL byte. | The spawn fails, exit 126, naming the argument. No truncation. |
| `path_under` and a string-prefix sibling directory. | Component-wise check after canonicalization. |

## Testing, by exposure

Tests are ordered by how much a defect would expose. The first four groups
are pure and table-driven (`rstest`); they should be dense.

1. Parse and render. Every flag form; `--` placement; each `Tail` mode;
   repeatable, choices, required, int; unknown verb, verb prefix, flag prefix,
   clustered and glued shorts; case sensitivity; the root-verb program.
2. Injection corpus. Values that look like flags, via literal and via
   variable; `--` inside a value; empty string; `=` inside a value; unicode;
   NUL.
3. `path_under`. `..` traversal; symlink out of root; absolute outside root;
   relative under cwd; cwd outside root; sibling-prefix directory; root that
   is itself a symlink; virtual cwd.
4. Validation binder agrees with the execution binder. The validator's
   `ToolArgs` and the executor's `ToolArgs` produce the same verdict for the
   same source. (See the two-binders gotcha in CLAUDE.md's lineage: validate
   from what execute reads.)
5. Environment. A pin overrides an exported variable of the same name; an
   unexported scope variable is absent; an OS environment variable set in the
   test process is absent from the child; a structured exported value fails.
6. Pinned executable. A script that sets `PATH` to an empty directory still
   runs the wrapper; `build()` fails for a missing path, a directory, a file
   without an execute bit, a relative path.
7. stdin. `Closed` with piped input fails loudly; `Pipe` streams; `sleep 1 |
   wrapped` does not deadlock.
8. Cancellation. `timeout 1 wrapped-sleep 10` kills the child; a background
   job's wrapped child joins the job's process group.
9. Feature and policy gates. Runs with `allow_external_commands = false`; the
   `--no-default-features` kernel compiles without the module.
10. Output. Oversize stdout spills like an external command's; a
    `json_output` verb binds typed through `$(…)`; invalid JSON from the child
    is an error.
11. Published text. `help <name>` renders verbs, flags, constraints, and the
    `Forward` marker; `kaish --plan` reports an unknown flag before execution.

## Open items

- Per-verb `json_output` and `ToolSchema::typed_substitution`: **decided.** The
  root schema sets `typed_substitution` only when *every* verb the tool can run
  declares `json_output()`. A tool with one JSON verb among text ones would
  otherwise make `$(cargo build)` bind a value that does not exist. Each verb's
  own schema still carries the flag, so `help` reports it per verb, and a caller
  who wants the data from one JSON verb among many asks with `--json`. Reading
  the leaf is a kernel change; it is not needed until a declaration wants a
  mixed tool to substitute typed.
- The declaration types may move to `kaish-types` if an embedder wants to
  deserialize declarations without the kernel. Not before one asks.
- Presets (`git`, `python`, `cargo`) ship as data outside the kernel. One small
  `git` declaration lives in-tree as the test fixture.
