# Ignore File Configuration

Ignore rules filter files during recursive walks in `glob`, `tree`, `ls -R`, `grep -r`, and `find`.

## Modes

| Mode | Scope | Defaults | Auto-gitignore | Effect |
|------|-------|----------|----------------|--------|
| REPL | Advisory | on | on | `glob`, `tree`, `ls`, and `grep` filter `.gitignore` and defaults. `find` walks everything. |
| Agent | Enforced | on | on | All file-walking tools filter, including `find`. |
| Embedded | Advisory | off | off | No filtering runs unless the embedder configures it. |

## Scope

- **Advisory:** `glob`, `tree`, `ls`, and `grep` apply ignore rules. `find` walks all paths.
- **Enforced:** Every file-walking tool applies ignore rules, including `find`.

Pass `--no-ignore` to `glob` or `tree` to bypass filtering for one call. Run `kaish-ignore clear` for a session-wide bypass.

## Default Ignore List

When defaults are on, tools always skip these directories: `.git`, `node_modules`, `target`, `__pycache__`, `.venv`, `venv`, `dist`, `build`, and `.next`.

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

Interactive mode filters files by default.
Run `kaish-ignore clear` in your session or add it to `~/.config/kaish/init.kai` to disable filtering:

```sh
kaish-ignore clear      # disable filtering for this session
```

Pass `--no-ignore` to bypass filtering on a single command:

```sh
glob '**/*' --no-ignore
```
