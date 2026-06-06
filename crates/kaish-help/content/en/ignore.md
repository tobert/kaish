# Ignore File Configuration

Controls which files are filtered by file-walking tools (glob, tree, ls -R, grep -r, find).

## Modes

| Mode | Scope | Defaults | Auto-gitignore | Effect |
|------|-------|----------|----------------|--------|
| REPL | Advisory | off | off | No filtering. `find` always unrestricted. |
| MCP | Enforced | on | on | All tools filter. Prevents context flooding. |

## Scope

- **Advisory** — polite tools (glob, tree, grep, ls) respect config. `find` ignores it (POSIX tradition).
- **Enforced** — all tools respect config, including `find`. Default for MCP/agent mode.

Per-tool `--no-ignore` flags override for that invocation.

## Default Ignore List

When defaults are on, these are always filtered: `.git`, `node_modules`, `target`, `__pycache__`, `.venv`, `venv`, `dist`, `build`, `.next`.

## kaish-ignore Builtin

```bash
kaish-ignore                          # show current config
kaish-ignore add .dockerignore        # add ignore file
kaish-ignore remove .gitignore        # remove from list
kaish-ignore clear                    # disable all filtering
kaish-ignore defaults on|off          # toggle default ignore list
kaish-ignore auto on|off              # toggle nested .gitignore loading
kaish-ignore scope advisory|enforced  # change scope
```

## REPL Setup

Add to `~/.kaishrc` for filtering in interactive mode:

```bash
kaish-ignore add .gitignore
kaish-ignore defaults on
kaish-ignore auto on
```
