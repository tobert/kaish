# Builtin sweep — compare/patch battery (cmp / diff / patch)

Fixed cells for the 3-vendor panel. File-oriented: the panel sees file contents
inline; the deferred runner materializes tempfile fixtures. cmp/diff are
universal; `patch` is a WRITER — its write-model safety is a kaibo `consult`
(source-grounded), not a panel cell. Temporary; delete with the sweep.

## fixtures
- **A** = `apple`⏎`banana`⏎`cherry`⏎
- **B** = `apple`⏎`BANANA`⏎`cherry`⏎   (line 2 differs)
- **C** = identical to A
- **P** (unified diff A→B):
  ```
  --- a
  +++ b
  @@ -1,3 +1,3 @@
   apple
  -banana
  +BANANA
   cherry
  ```

| ID | program | question |
|----|---------|----------|
| CM1 | `cmp A C` | output + exit code (identical)? |
| CM2 | `cmp A B` | exact message + exit code (differ)? |
| CM3 | `cmp -s A B` | output + exit code (silent)? |
| DF1 | `diff A C` | output + exit code (identical)? |
| DF2 | `diff A B` | **DEFAULT format** — what does plain `diff` emit, and exit code? |
| DF3 | `diff -u A B` | unified diff body + exit code? |
| DF4 | `diff -q A B` | output + exit code? |
| PT1 | `patch A < P` | what is printed; what does A contain after; exit code? |
| PT2 | `patch --dry-run A < P` | output; is A modified? |

Probes: **CM*/DF* exit codes** (POSIX 0/1/2 — agents gate on these); **DF2
default format** (kaish defaults to unified; GNU `diff` defaults to ed-style
"normal" — divergence from muscle memory); **PT* write model** (covered by the
consult below).

## write-model safety — kaibo consult (source-grounded, not panel)
Question for DeepSeek: do `patch` (and `tee`) route file writes through kaish's
VFS / overlay / latch / trash write surface, or write directly to disk? This is
the wave-3 safety headline — a writer that bypasses the confirmation/overlay
machinery is the same hazard class as sed's deferred `-i`.
