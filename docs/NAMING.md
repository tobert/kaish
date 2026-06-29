# Naming Conventions

## Variables

| Namespace | Owner | Example | Purpose |
|-----------|-------|---------|---------|
| `KAISH_*` | Shell | `KAISH_VERSION`, `KAISH_PID` | Shell-provided, read-only by convention |
| `__KAISH_*__` | Compiler | `__KAISH_ARITH_0__` | Internal compiler/lexer markers, never user-visible |
| `_PREFIXED` | User | `_EXTERNAL` | Skips undefined-variable warnings (external/unchecked convention) |
| Everything else | User | `MY_VAR`, `count` | Normal user variables |

## Functions

| Namespace | Owner | Example | Purpose |
|-----------|-------|---------|---------|
| `kaish_*` | Hook contract | `kaish_prompt` | Well-known hook functions called by the REPL/kernel |
| Everything else | User | `deploy`, `build_all` | Normal user functions |

## Hook Functions

The REPL calls these if defined. All are optional.

| Function | When | Contract |
|----------|------|----------|
| `kaish_prompt` | Before displaying prompt | stdout becomes the prompt string |
| `kaish_pre_command` | Before each command executes | Informational; cannot modify the command |
| `kaish_post_command` | After each command completes | Has access to `$?` exit status |

## Builtins

| Namespace | Example | Purpose |
|-----------|---------|---------|
| `kaish-*` | `kaish-vars`, `kaish-tools`, `kaish-mounts`, `kaish-validate` | Builtins that introspect or operate on kaish's own runtime/machinery |
| Everything else | `echo`, `grep`, `ls`, `jq` | Standard utilities and external command wrappers |

The `kaish-` prefix on builtins signals "this is about the shell itself, not a general utility." Use it for tools that expose kernel state, inspect the runtime, or control kaish-specific features.

### Shared flag conventions

When a flag means the same thing across builtins, it gets one name everywhere — so an agent that learns it once is never wrong on another tool:

- **`--ftype <NAME>` / `--ftype-not <NAME>` / `--ftype-list`** — rg-style *file-type* filtering (by extension/name, e.g. `--ftype rust` → `*.rs`), on every search builtin (`grep`, `glob`). Deliberately distinct from `-t`/`--type`, which on `find`/`glob` is *entry kind* (file/dir/symlink, the POSIX/fd convention) — two different axes, two names. A file-type filter narrows files only; directories pass it untouched.

## Style Rules

- Full words, no abbreviations: `kaish_post_command` not `kaish_postcmd`
- Underscores between words: `pre_command` not `precommand`
- User functions and variables: any name that doesn't start with `kaish_` or `KAISH_`
