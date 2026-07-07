# Ignore File Configuration

Controls which files are filtered by file-walking tools (glob, tree, ls -R, grep -r, find).

## Modes

| Mode | Scope | Defaults | Auto-gitignore | Effect |
|------|-------|----------|----------------|--------|
| REPL | Advisory | on | on | Polite tools filter (.gitignore + defaults); `find` unrestricted. Recover per call with `--no-ignore`, per session with `kaish-ignore clear`. |
| Agent | Enforced | on | on | All tools filter. Prevents context flooding. |
| Embedded | Advisory | off | off | No filtering unless the embedder configures it. |

## Scope

- **Advisory** — polite tools (glob, tree, grep, ls) respect config. `find` ignores it (POSIX tradition).
- **Enforced** — all tools respect config, including `find`. Default for agent (embedded) mode.

Per-tool `--no-ignore` flags override for that invocation.

## Default Ignore List

When defaults are on, these are always filtered: `.git`, `node_modules`, `target`, `__pycache__`, `.venv`, `venv`, `dist`, `build`, `.next`.

## kaish-ignore Builtin

```sh
kaish-ignore                          # show current config
kaish-ignore add .dockerignore        # add ignore file
kaish-ignore remove .gitignore        # remove from list
kaish-ignore clear                    # disable all filtering
kaish-ignore defaults on|off          # toggle default ignore list
kaish-ignore auto on|off              # toggle nested .gitignore loading
kaish-ignore scope advisory|enforced  # change scope
```

## REPL Opt-Out

Interactive mode filters by default. To see everything, add to
`~/.config/kaish/init.kai` (or run ad hoc):

```sh
kaish-ignore clear      # this session: no filtering at all
```

or reach past the filter per call: `glob '**/*' --no-ignore`.
