# OverlayFs for kaish-vfs — design proposal

Drafted in kaibo (解剖), destined for kaish: a copy-on-write overlay filesystem for
`kaish-vfs`, sitting beside `LocalFs` and composing with `MemoryFs`. Writes land in
an upper layer; the lower layer is never touched. The overlay knows exactly what
changed, when, and from what — so a consumer can render a patch, commit, fork, or
discard.

The one-line pitch: **an overlay makes any kaish session a transaction.**

## Motivating consumers (three, today)

1. **kaibo coder workspaces.** kaibo's planned `coder` tool lets a model edit code
   in a sandbox and return a unified diff, without ever weakening kaibo's
   "read-only is the product" invariant. The model edits the overlay; kaish reads
   (`cat`/`rg`) see the edited state; the real tree stays byte-identical. The
   driver reviews and applies the patch with its own tools. Needs: copy-up with
   **base snapshots** (the diff must be computed against the content as it was at
   first edit, so the patch stays coherent even if the real tree moves underneath —
   and `git apply` then fails loudly on a moved tree, which is the right failure).
2. **kaijutsu context forking.** kaijutsu's kernel forks contexts in a DAG; a
   forkable overlay is the filesystem twin — fork a context, fork its overlay, and
   parallel agents each see their own pending edits without merging raw bytes.
   Needs: a cheap `fork()` (shared lower, cloned upper).
3. **kaish `--overlay` session mode.** Run a destructive-looking pipeline, inspect
   what it *would have* done (`kaish-vfs diff`), then commit or drop. Opt-in on
   every frontend — silently making writes virtual would surprise both human users
   and agent operators, and surprise is what kaish exists to remove. Needs:
   `kaish-vfs` builtin with subcommands riding the inspection API.

## Where it sits

- **Crate: `kaish-vfs`** (the trait's home, leaf, runtime-light). Not the kernel:
  consumers like kaijutsu shouldn't need the kernel to get the overlay.
- **Generic upper.** `OverlayFs::new(lower: Arc<dyn Filesystem>, upper: Arc<dyn Filesystem>)`
  with `MemoryFs` as the conventional upper. The overlay tracks dirtiness itself
  (it sees every mutation pass through its own methods), so the upper needs no
  special hooks — and a `LocalFs` scratch-dir upper gets you a disk-backed
  (restart-surviving) overlay for free later. Note: `MemoryFs` currently lives in
  `kaish-kernel/src/vfs/memory.rs`; either lift it into `kaish-vfs` (its only
  non-std dep is `tokio::sync`, already optional in the crate) or leave it and let
  the kernel wire the conventional pairing. **Decided: lift it** — new `memory`
  feature on `kaish-vfs` (= `tokio/sync`), kernel re-exports it the same way it
  already re-exports `LocalFs`. Enables an `OverlayFs::over(lower)` convenience
  constructor with the conventional `MemoryFs` upper.
- **Mounting.** Consumers mount it like any `Filesystem` via `VfsRouter` (longest
  prefix). kaibo would replace its `LocalFs::read_only` project mount with
  `OverlayFs::over(LocalFs::read_only(root))` when a workspace wants writes.

## State

```rust
pub struct OverlayFs {
    lower: Arc<dyn Filesystem>,
    upper: Arc<dyn Filesystem>,            // writable; MemoryFs by convention
    state: RwLock<OverlayState>,
}

struct OverlayState {
    /// Paths deleted relative to lower. A later write clears the whiteout.
    whiteouts: HashSet<PathBuf>,
    /// First-touch snapshots: lower content at the moment a path first became
    /// dirty. `None` = the path did not exist in lower (it's an Added file).
    bases: HashMap<PathBuf, Option<Vec<u8>>>,
}
```

`bases` is the inspection API's ground truth and is owned by the overlay, not the
upper — the upper holds *current* content only. (Persisting `bases` alongside a
disk-backed upper is a later concern; note it, don't build it.)

## Semantics, per trait method

| op | behavior |
|---|---|
| `read` | whiteout → `NotFound`; upper hit → upper; else lower. |
| `write` | first touch of a lower path → record base snapshot; clear any whiteout; write upper. |
| `list` | union(lower, upper) minus whiteouts; upper wins name collisions. Dirs may exist in either layer. |
| `stat` / `exists` | same precedence as `read`. |
| `mkdir` | upper only. (A dir that exists in lower is already visible; mkdir-over-it is the usual `Ok` for create-parents semantics.) |
| `remove` | upper-resident → remove from upper; lower-resident → record base, add whiteout. Removing a non-empty dir follows lower's error contract. |
| `rename` | copy-up source (read through overlay), write dest, remove source (which whiteouts a lower source). The trait's default copy+delete fallback is nearly right already; an explicit impl keeps base bookkeeping correct: dest gets `base=None` *unless dest already existed* (then its base snapshots first). Directories: `Unsupported` initially, matching the trait default. |
| `set_mtime` | upper-resident → upper. Lower-resident → treat as a mutation: copy up (content + base), then set on upper. No silent no-op, per the trait's contract. |
| `read_only` | `false` — that's the point. |
| `real_path` | **`None`, always.** Returning lower's real path for clean files would hand a tool a host path that bypasses the overlay (external commands like `git` write through real paths). Correctness beats convenience here; a consumer that needs real paths materializes (below). Concretely: external commands (`git`, `cargo`, …) can't run with cwd inside an overlay mount, so committing *through* the overlay is impossible — correct and intended, since that's exactly what the overlay exists to prevent. |
| `read_link` / `symlink` / `lstat` | delegate by the same precedence; `symlink` goes to upper (`MemoryFs` supports symlinks). Symlinked *escape* is the lower's problem (`LocalFs::read_only` already polices its root), not the overlay's. Symlinks created in the upper are tracked as dirty state that blocks `changes()`/`commit_into` (Unsupported, loud, never silent). |

## Inspection API (the consumer-driven part — this is why it's not just MemoryFs)

```rust
pub enum ChangeKind { Added, Modified, Removed }   // Debug, Clone, Copy, PartialEq, Eq

pub struct OverlayChange {                          // Debug, Clone
    pub path: PathBuf,
    pub kind: ChangeKind,
    /// Lower content at first touch; None for Added.
    pub base: Option<Vec<u8>>,
    /// Current overlay content; None for Removed.
    pub current: Option<Vec<u8>>,
}

impl OverlayFs {
    /// Returns Err(Unsupported) if dirty symlinks are present.
    pub async fn changes(&self) -> io::Result<Vec<OverlayChange>>;
    pub async fn is_dirty(&self) -> bool;
    /// Drop one path's edits (restore lower visibility).
    pub async fn reset(&self, path: &Path) -> io::Result<()>;
    /// Drop all edits; returns Err on I/O failure (overlay stays consistent).
    pub async fn reset_all(&self) -> io::Result<()>;
    /// Write the dirty set into a writable target (e.g. the real tree for
    /// `vfs-commit`, or a scratch dir for materialization). Refuses if a base
    /// snapshot no longer matches the target's current content — stale-base
    /// detection is a loud error, not a silent overwrite. For Added files the
    /// check inverts: the target path must not exist (a file appearing where
    /// we added one is a conflict, reported loudly).
    /// target must not be this overlay or wrap it (deadlock via held read lock).
    pub async fn commit_into(&self, target: &dyn Filesystem) -> io::Result<()>;
    /// Fork: shared lower, dirty set copied into a caller-supplied fresh upper
    /// (kaijutsu's context fork). fresh_upper must be empty and must not be
    /// the same Arc as self.upper. Charges the budget for cloned bases up-front.
    pub async fn fork_into(&self, fresh_upper: Arc<dyn Filesystem>) -> io::Result<OverlayFs>;
}
```

**File-content scope (pinned for v1).** `OverlayChange.base/current` are byte
vectors, so `changes()` reports *files only*. `commit_into` creates parent
directories implicitly; empty added directories are not committed. A dirty
symlink (`MemoryFs` supports them, so this is reachable) makes `commit_into`
and `changes()` error `Unsupported` loudly — upgradeable later, never silent.
This covers both symlinks created in the upper via `symlink()` and lower-resident
symlinks removed via `remove()`.

**Commit atomicity (pinned).** `commit_into` is not transactional: it
pre-flights *all* base checks first, then writes. A mid-write failure is still
possible (target mutates between check and write, I/O error); it fails loudly
reporting which paths landed. Narrowing the window without pretending the trait
gives us transactions.

**Consumer note.** `commit_into` does NOT mutate overlay state — the overlay
stays dirty after a successful commit. Callers that want a clean overlay call
`reset_all()` after a successful commit. `commit_into` writes file content only;
timestamps are not propagated — the target's mtime becomes "now" for written
files. A `set_mtime`-only `Modified` (base == current) commits identical bytes
without transferring the pinned time.

**Directory asymmetry.** `is_dirty()` returns `true` if any whiteouts are
present, even if `changes()` would return an empty `Vec` (e.g. a whiteouted
lower *directory* has no base entry). A `mkdir`-only session reports clean
(no whiteouts, no bases). `commit_into` never deletes directories in the target
and never adds empty added directories.

**`fork_into` budget.** Forked bases charge the inherited budget up-front —
forked bases are real RAM drawn from the same pool as the parent. `base_bytes`
counts unconditionally (even without a budget): it always equals the sum of all
`Some(base)` lengths in `bases`.

**Drop credits the budget.** Dropping a budgeted `OverlayFs` credits its base
bytes; dropping a budgeted `MemoryFs` credits its content bytes. Without this,
fork-then-drop and workspace create-then-drop cycles would drain the shared
pool toward spurious `StorageFull` with zero actual RAM held.

Diff *rendering* (unified diff text) stays out of the core: kaibo renders with the
`similar` crate; a kaish `vfs-diff` builtin would render kernel-side. The overlay's
job is to hand back `(base, current)` pairs that make rendering trivial.

Mutation-during-`changes()` is torn-read territory: take the state lock for the
whole snapshot.

`fork_into()` cost is `O(dirty set)` (copy the upper's dirty files + clone the
state maps). Fine for code-editing workloads; if kaijutsu ever forks overlays
with huge dirty sets, an `im`-style persistent map upper is the upgrade path —
don't pre-build it.

## Failing-first tests (the boundary gets teeth)

The read-only guarantee below the overlay must be *proved*, kaibo-style — including
at least one test that demonstrates the harness can fail (e.g. point it at a
writable lower and watch the byte-identical assertion trip).

1. Write through overlay → **real file on disk byte-identical** (the invariant).
2. Read-after-write coherence: `cat` sees the edit; `rg` finds the new symbol.
3. `remove` a lower file → `exists` false, `list` omits it, disk untouched.
4. Write after remove → whiteout cleared, kind = Modified, base = the original
   (**pinned**: the user-visible story is "I replaced the file", so the base
   survives the remove/re-create round trip and the diff is against the
   original).
5. `mkdir` + write into a new dir; merged `list` shows both layers; collision →
   upper wins.
6. Rename lower → new path: dest readable, source whiteouted, disk untouched,
   bases correct on both ends.
7. `changes()` kinds and bases exact: Added (no lower), Modified (snapshot, not
   current lower), Removed (base = content at remove time).
8. Lower drifts after copy-up → `changes()` still reports the *snapshot* base;
   `commit_into` a drifted target → loud stale-base error.
9. `reset(path)` restores lower visibility; `reset_all` leaves `is_dirty()` false.
10. `fork_into()` independence: edits after the fork don't bleed either direction.
11. `set_mtime` on a lower file copies up rather than erroring or silently lying.
12. Strict-glob interaction: a glob over a merged dir matches union-minus-whiteouts
    (globs resolve through `list`, so this should fall out — prove it anyway).
13. Path normalization: whiteouts and bases key on `PathBuf`, so `foo/./bar` and
    `foo/bar` must hit the same entry — a whiteout recorded under one spelling
    must not be missed under another.

## Performance notes

- Reads of clean files are one upper miss + one lower read — negligible over
  `LocalFs`.
- Memory is `O(dirty content + base snapshots)` ≈ 2× the edited bytes — **never
  O(tree)**; the lower layer is the live disk, read on demand. Code edits are
  small; a workload that rewrites a 500 MB asset through the overlay is holding
  ~1 GB with a `MemoryFs` upper — see Storage & memory below for the escape.

## Storage & memory

The generic upper is the whole memory strategy — pick the upper to pick the
cost model:

- **`MemoryFs` upper**: zero disk writes, strictly ephemeral, RAM = 2× edited
  bytes. Right default for one-shot/audit-sensitive uses.
- **`LocalFs` upper** rooted in consumer-owned state (e.g.
  `$XDG_STATE_HOME/<app>/workspaces/<id>/upper/`, bases beside it): dirty set
  lives on disk, hot files ride the OS page cache (RAM-speed reads, eviction
  under pressure, kernel-managed). This is deliberately *not* a custom mmap
  arena — plain files + page cache give the same residency behavior without
  growing-file/free-space/crash-consistency machinery. Side effect: overlays
  survive a restart, which consumers wanting durable workspaces get for free.
- **Tree copies (reflink or otherwise) are a non-goal** for the overlay: copying
  changes semantics (snapshot isolation — reads stop tracking the live lower),
  and the one place a real materialized tree is needed (running compilers over
  the merged view) is `O(dirty)` anyway: expose the lower read-only and bind the
  dirty set over it; `commit_into(scratch)` is the overlay-side half.

### Byte accounting: always-on counting, profile-defaulted limits

Counting and limiting are different concerns; split them:

- **Counting is always on, inside each memory-resident fs.** The trait grows
  `fn resident_bytes(&self) -> Option<u64>` (default `None`); `MemoryFs` and
  `OverlayFs` maintain an exact internal counter — *net*, not gross: an
  overwrite charges the delta, a remove credits, all under the lock that
  already guards the entry map (only the fs itself knows the old size, so
  counting anywhere else — router, wrapper — is racy or gross-only). Cost is
  an integer update next to a `HashMap` insert: noise. `LocalFs` stays `None`
  — disk residency is the host's concern (page cache, `df`); this counter is
  about RAM. `OverlayFs` counts its **base snapshots** too — overlay-owned RAM
  regardless of upper choice, the hidden 2×. `JobFs` is intentionally excluded:
  it synthesizes a filesystem over already-bounded `BoundedStream` ring buffers
  that are size-capped independently; double-counting those bytes against the
  `vfs-memory` budget would be incorrect. Zero config, no wrapper to remember:
  unbounded growth becomes structurally impossible to *miss*, which is the
  kaish way.
- **Limiting is a shared `ByteBudget`** (`Arc`: atomic used + limit), accepted
  at construction (`MemoryFs::with_budget`, `OverlayFs::with_budget`) and
  charged by every fs holding it — one handle across a kernel's `/` scratch and
  a workspace upper gives one number per kernel. Exceeding it fails the write
  loudly, ENOSPC-style (`StorageFull`, message naming the budget and the knob)
  — an in-band error a model reads and adapts to; fail loud over quietly eating
  RAM. Defaults ride kernel profiles exactly like `OutputLimitConfig` already
  does: `KernelConfig::agent()` gets a conservative default budget,
  interactive/host profiles generous or none. That flips the polarity —
  embedders are protected by default and opt *out* knowingly — at the cost of
  one loud, documented behavior change for existing embedders.
  `KernelConfig::agent()` and `agent_with_root()` default to a 64 MiB budget
  with the label `"vfs-memory"`. Opt out via `without_vfs_budget()` or
  override with `with_vfs_budget(limit, label)`.
- **Introspection rides the counters**: a `df --json`-style surface (per mount:
  `resident_bytes`, budget used/remaining) feeds `workspace list --json`,
  LRU/idle eviction on real numbers, and lets a driving agent manage its own
  footprint.

Evidence this isn't theoretical: kaibo's live probe pushed 17.4 MB into an
unquota'd `/` `MemoryFs` in ~1 s, exit 0, via a redirect loop (kaibo
`docs/issues.md` P2, 2026-06-10). The same probe *cleared* the output-spill
path: `SpillMode::Memory` truncates (bounded), so scratch writes are the one
unbounded path. This design supersedes the `QuotaFs` wrapper idea from the
first draft: a wrapper undercounts (it can't see overlay bases), stacks
identity-obscuring layers, and is opt-in — exactly the kind of guard that
drifts.
- Compilers/linters can't read a VFS. Consumers that need one **materialize**: the
  cheap shape is the real tree exposed read-only (bind mount or as-is) with only
  the dirty set written to a scratch dir overlaid on top — `O(dirty)`, not
  `O(tree)`, so big projects cost what they edited, not what they are.
  `commit_into(scratch LocalFs)` is the overlay-side half of that story.

## Settled forks (2026-06-10)

- **MemoryFs's home**: lifted into `kaish-vfs` behind a `memory` feature
  (= `tokio/sync`); kernel re-exports, same pattern as `LocalFs`.
- **Fork API**: `fork_into(fresh_upper)` — caller supplies the empty upper
  (a `dyn Filesystem` can't be cloned generically); O(dirty) copy via trait
  methods.
- **Remove-then-rewrite**: kind = Modified, base = original (test 4).
- **changes()/commit_into scope**: files only; implicit parent dirs; dirty
  symlinks error `Unsupported` loudly.
- **Commit atomicity**: pre-flight all base checks, then write; loud partial
  failure, not transactional.
- **`--overlay` surface** (settled 2026-06-10): opt-in on every frontend —
  never default-on anywhere.
  - `kaish --overlay` — interactive REPL
  - `kaish --overlay -c 'script'` — one-shot
  - `KernelConfig::with_overlay(true)` — embedder API (an embedder, e.g. an MCP
    server like kaibo/kaijutsu, exposes it however it likes)
  - **Per-call semantics (agent embedders)**: an agent embedder typically creates
    a fresh kernel per `execute()` call, each with a fresh overlay transaction.
    `kaish-vfs commit` must run in the **same call** as the writes — if you write
    in call N and commit in call N+1, the transaction from call N was discarded
    when its kernel was dropped. This is why default-on was rejected for that
    pattern: a model that issues writes and commits in separate calls would
    silently lose every write.
- **Builtin name** (settled): `kaish-vfs` with subcommands, superseding the
  earlier design names `vfs-diff` / `vfs-commit`. Subcommands: `status`,
  `diff [path...]`, `commit`, `reset [path]`.
- **JobFs resident counting**: intentionally excluded from `resident_bytes()`
  and the `vfs-memory` budget. JobFs synthesizes a filesystem over already-bounded
  `BoundedStream` ring buffers; those bytes are accounted separately.
- **Budget defaults**: `KernelConfig::agent()` / `agent_with_root()` default to
  64 MiB with label `"vfs-memory"`. Opt out with `without_vfs_budget()`.

## Open forks (still)

- **Whiteout of a directory**: whole-subtree whiteout vs. per-file (per-file is
  simpler and matches `remove`'s empty-dir contract; subtree can come later).
- **Base persistence** for disk-backed uppers (only matters when someone wants
  restart-surviving overlays; kaibo will, eventually — "backing store later").
