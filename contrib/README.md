# contrib/

Shell completion scripts generated from kaish's clap_derive structs.

## echo.bash

Bash completion for the `echo` builtin. Source it in your shell:

```bash
source contrib/echo.bash
```

Regenerate after the clap struct changes:

```bash
cargo run --example gen_bash_completion -p kaish-kernel > contrib/echo.bash
```

This is a **proof-of-life** that the clap-derived builtin argv layer
gives us shell completions for free. Every
builtin's argv layer is a `clap::Command`, so `clap_complete` can
generate completions for any of them — `echo` is just the smallest
example. Future work: a single `kaish-complete` generator that walks
the tool registry and emits a unified completion script for every
builtin.
