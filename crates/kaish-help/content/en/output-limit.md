# Output Size Limits

Output limits cap captured command output. Output over the cap spills to a file and returns a head preview, a tail preview, and the spill path. See Tail-Only Spills for outputs exceeding 10MB.

## Modes

| Mode | Enabled by default | Limit | Head | Tail |
|---|---|---|---|---|
| REPL | off | unlimited | 1K | 512 |
| Agent | on | 8K | 1K | 512 |

The REPL starts without a limit. Run `set -o output-limit` to enable it, or add that line to `~/.kaishrc`.

## How It Works

1. The command runs and produces output.
2. When output exceeds `max-bytes`, kaish writes the captured bytes to a spill file.
3. kaish replaces the output with a head preview, `...`, a tail preview, and the spill path.
4. Read the spill file selectively with `cat`, `head`, or `tail`.

## Exit Code on Spill

- A spill always exits `3`.
- An external command that overflows the 10MB capture buffer also exits `3`, even with `output-limit` off.
- Disable the limit before reading the spill file so the read does not spill again:

```sh
set +o output-limit
cat /run/user/1000/kaish/spill/spill-1234567890.123-4567.txt
set -o output-limit=8K
```

## Spill Files

Spill files live in `$XDG_RUNTIME_DIR/kaish/spill/`, typically `/run/user/$UID/kaish/spill/`. When `$XDG_RUNTIME_DIR` is unset, kaish falls back to the system temp directory.

- Stored on RAM-backed tmpfs on systemd systems.
- Cleared automatically on reboot.
- Scoped to the current user without shared permissions.

When configured for in-memory truncation (including `NoLocal` builds), kaish writes no spill file and appends `[output truncated in memory: … — no spill file]`.

## kaish-output-limit Builtin

```sh
kaish-output-limit              # show current config
kaish-output-limit set 64K      # set limit using K or M suffixes or raw bytes
kaish-output-limit on           # enable with default 8K limit
kaish-output-limit off          # disable limit
kaish-output-limit head 2048    # set head preview size
kaish-output-limit tail 1024    # set tail preview size
```

## Truncated Output Format

```
<first 1024 bytes of output>
...
<last 512 bytes of output>
[output truncated: 234567 bytes total — full output at /run/user/1000/kaish/spill/spill-1234567890.123-4567.txt]
```

Here `234567 bytes total` is the exact command output size, and the spill file contains every byte.

## Tail-Only Spills

kaish captures external command `stdout` in a fixed 10MB buffer of `10485760` bytes regardless of `max-bytes`. Output past 10MB drops the earliest bytes before spilling, so the spill file holds only the tail. The preview marker reports `captured` instead of `total` to show how many bytes survived:

```
<first 1024 bytes of output>
...
<last 512 bytes of output>
[output truncated: 10485760 bytes captured — tail only at /run/user/1000/kaish/spill/spill-1234567890.123-4567.txt; earlier output was dropped before the spill, so see the stderr overflow marker for the true size]
```

The `stderr` overflow marker reports the exact lost and total bytes:

```
[stdout truncated: output exceeded the 10MB capture buffer — first 4194304 bytes lost (14680064 bytes total written); enable output-limit to spill to disk]
```

Lost bytes cannot be recovered. Rerun the command with `grep`, `head`, or a file redirect to capture earlier output.
