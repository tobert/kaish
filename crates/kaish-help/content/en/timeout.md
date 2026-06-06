# `timeout` — Run a command with a deadline (kills the child on elapsed)

## Syntax

```
timeout DURATION COMMAND [ARGS...]
```

## Duration formats

`30` (seconds), `30s`, `500ms`, `5m`, `1h`. Pure numbers and the `s` suffix
accept decimals (`1.5`, `0.25s`).

## Behavior

On elapsed:

1. The kernel cancels the inner command's execution token.
2. Any running external child receives **SIGTERM** to its process group.
3. The kernel waits up to `KernelConfig::kill_grace` (default 2s) for the
   child to exit cleanly.
4. If the child is still alive, the process group receives **SIGKILL**.
5. `timeout` returns exit code **124** (matching coreutils convention) and
   writes `timeout: timed out after <duration>` to stderr.

When the inner command finishes within the deadline, `timeout` returns the
inner command's exit code unchanged.

## Examples

```bash
timeout 5 sleep 10                  # exits 124 after ~5s; sleep is killed
timeout 500ms curl example.com      # bounds a network call
timeout 2m cargo build              # bounds a long build
```

## Notes

- The kill applies to the **entire process group** of the spawned child, so
  shell wrappers (`bash -c '...'`) do not protect their grandchildren.
- For a process that traps SIGTERM and ignores it, the SIGKILL escalation
  after `kill_grace` is uncatchable.
- The same kill discipline applies to `KernelConfig::request_timeout`
  (kernel-wide default), `ExecuteOptions::timeout` (per-call), and
  `Kernel::cancel()` (embedder-initiated).
