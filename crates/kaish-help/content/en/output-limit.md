# Output Size Limits

Controls maximum command output size. When exceeded, full output is written to a spill file and the result is truncated with a pointer.

## Modes

| Mode | Enabled by default | Limit | Head | Tail |
|------|---------|-------|------|------|
| REPL | off | unlimited | 1K | 512 |
| MCP | on | 64K | 1K | 512 |

The REPL starts with no limit but `set -o output-limit` works at any time. To make it persistent, add it to `~/.kaishrc`.

## How It Works

1. Command runs and produces output
2. If output exceeds `max-bytes`, full output is written to a spill file
3. Result is replaced with: head preview + `...` + tail preview + pointer to spill file
4. Agent can read the spill file selectively with `cat` or `head`/`tail`

## Exit Code on Spill

Spill always exits **3**. The spill file path is shown in the output. To read it without hitting the limit again:

```bash
set +o output-limit
cat /run/user/1000/kaish/spill/spill-1234567890.123-4567.txt
set -o output-limit=8K
```

## Spill Files

Location: `$XDG_RUNTIME_DIR/kaish/spill/` (typically `/run/user/$UID/kaish/spill/`)

- RAM-backed tmpfs on systemd systems
- Cleared on reboot
- User-scoped (no permission issues)

## kaish-output-limit Builtin

```bash
kaish-output-limit                    # show current config
kaish-output-limit set 64K            # set limit (K/M suffixes or raw bytes)
kaish-output-limit on                 # enable with default 64K limit
kaish-output-limit off                # disable (unlimited)
kaish-output-limit head 2048          # set head preview size
kaish-output-limit tail 1024          # set tail preview size
```

## Truncated Output Format

```
<first 1024 bytes of output>
...
<last 512 bytes of output>
[output truncated: 234567 bytes total — full output at /run/user/1000/kaish/spill/spill-1234567890.123-4567.txt]
```
