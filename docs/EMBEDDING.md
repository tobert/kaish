# Embedding kaish

This guide shows how to embed the kaish kernel in your application: kernel
construction, capability features, per-call execution options, custom tools,
and output capture.

## Stability

kaish is pre-1.0 (currently 0.13.x, MSRV 1.85). The language has settled;
the embedding API may still change between minor versions where it improves
both kaish and its embedders — [kaijutsu](https://github.com/tobert/kaijutsu)
is the reference embedder. Pin a minor version and read release notes when
bumping.

**Panic safety:** kaish makes no panic-unwind guarantees. Errors returned as
`Err(...)` always clean up; a panic mid-execute may leave kernel state (e.g.
a pushed scope frame) behind. Treat a panicking kernel as poisoned.

## Quick Start

```rust
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create a kernel with defaults
    let kernel = Kernel::new(KernelConfig::transient())?;

    // execute() returns Ok(ExecResult) even when the script fails
    // (nonzero exit code); Err(_) is reserved for kernel faults.
    let result = kernel.execute("echo 'Hello from kaish!'").await?;
    if result.code != 0 {
        eprintln!("script failed: {}", result.err);
    }
    println!("{}", result.text_out());

    Ok(())
}
```

`ExecResult` exposes stdout via the `text_out()` accessor (it materializes
structured output when a builtin returned a table or tree); `code`, `err`,
and `data` are public fields.

## The result contract

Output is clean text by default — simple commands return plain text, structured
builtins (`ls`, `kaish-mounts`, `kaish-vars`) render readable tab-separated
values, and `--json` on any command emits JSON plus a parsed value (`data`) that
builtins set explicitly — kaish never infers it by sniffing stdout. The exit
code is something agents can branch on:

| `code` | Meaning | Recovery |
|--------|---------|----------|
| 0 | Success | — |
| 1 | Failure | Read `err` |
| 2 | Approval required (`set -o approvals`) | Grant the request, then re-run with `--confirm="<token>"` — embedders read the typed `ExecResult.approval` (or call `Kernel::confirm`); the request's `hint` shows the re-run for humans |
| 3 | Output truncated by the output limit | `original_code` holds the real exit code. With disk spill the message names the spill file — `cat` it, or narrow the query; memory-spill kernels (`with_backend`, `SpillMode::Memory`) truncate in place with no file |
| 124 | Timeout (`timeout_ms`, default 30 s) | — |
| 130 | Cancelled | — |

**Assert on the code and the kind, never on the wording.** The exit code is
contract, and so is the `std::io::ErrorKind` a VFS refusal carries — a write
anywhere under `/v/approvals` is `ErrorKind::Unsupported` (not
`PermissionDenied`: no permission would make it work) and the statement exits
**1**. The text in `err` is prose written for a human and improves between
releases without notice, so a boundary test that pins it fails on a wording
change that changed no behavior.

Embedders typically run a fresh kernel per request (variables, functions,
aliases, `set -o` options, and `cwd` reset each time) while trash and
approval requests persist across calls — share one ledger with
`KernelConfig::with_approver_handle()` (see
[Destructive-op rails](#destructive-op-rails-inspecting-and-fulfilling-an-approval-gate)).
An undecided request lives until you decide it or cancel it: kaish does not expire
requests, because how long one should live is yours to choose.

## Stack size — size your execution threads

The interpreter recurses on the **native stack**: command substitution
(`$(…)`), shell-function calls, and `.kai` script sourcing all re-enter the
statement engine. A runaway or mutually recursive script is caught by a depth
guard ([`MAX_RECURSION_DEPTH`], 48) that returns a loud
`"maximum recursion depth exceeded"` error instead of overflowing the stack —
**but the guard only fires *before* the overflow if the thread has enough
stack.** On the default ~2 MB tokio worker stack, a deep recursion SIGSEGVs
before reaching the cap.

kaish can't set this itself (it doesn't own your runtime), so it exposes the
floor: **[`RECOMMENDED_STACK_SIZE`] (12 MiB)**. The cap and the floor are a
matched pair — the floor is sized so the guard trips before `cap × per-level
stack` can overflow it. Size every thread that drives kaish execution to at
least this:

```rust
// Worker threads (pipeline stages, background jobs, scatter workers run here):
let runtime = tokio::runtime::Builder::new_multi_thread()
    .thread_stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
    .enable_all()
    .build()?;

// The block_on / driver thread also runs foreground recursion — tokio doesn't
// own it, so if it's the OS main thread (~8 MB) give it a sized std::thread:
std::thread::Builder::new()
    .stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
    .spawn(move || runtime.block_on(async { /* … kernel.execute … */ }))?
    .join().unwrap();
```

Below the floor the guard still bounds *most* recursion, but a deep foreground
recursion on an undersized driver thread can still overflow — the reference
REPL (`kaish-repl`) sizes both its runtime workers and its driver thread to
`RECOMMENDED_STACK_SIZE`, and is the working example.

> **Debug builds pay more per level.** The GH #48 allocation pass cut the
> per-level stack to ~50 KB (release) / ~57 KB (this workspace's debug, which
> builds the interpreter crates at `opt-level = 1`). That profile setting lives
> in kaish's own `Cargo.toml` and does **not** propagate to your build — your
> *debug* build of the kernel pays the full unoptimized ~193 KB/level. The
> 12 MiB floor is deliberately sized against that worst case (48 × 193 KB ≈
> 9.3 MB), so you're covered either way; if you want the smaller debug frames
> too, add `[profile.dev.package.kaish-kernel] opt-level = 1` to your own
> workspace.

## Architecture

kaish separates concerns into layers:

```text
┌─────────────────────────────────────────────────────────┐
│  Your Application (e.g., kaijutsu)                       │
├─────────────────────────────────────────────────────────┤
│  KernelClient trait (kaish-client)                       │
│  - execute / tool_schemas / list_vars / cancel           │
│  - EmbeddedClient wraps an in-process Kernel             │
├─────────────────────────────────────────────────────────┤
│  KernelBackend trait (kaish-tool-api)                    │
│  - resolve_real_path() → maps VFS paths to real paths    │
│  - File operations, tool dispatch, mounts                │
├─────────────────────────────────────────────────────────┤
│  Kernel (kaish-kernel)                                   │
│  - Lexer/Parser/Validator/Interpreter                    │
│  - Tool Registry (builtins + custom tools)               │
│  - VFS Router                                            │
└─────────────────────────────────────────────────────────┘
```

Two ways in:

- **`Kernel` directly** — full surface, in-process.
- **`KernelClient`** (`kaish-client` crate) — the frontend trait the REPL
  drives; implement or reuse `EmbeddedClient::new(kernel)` if your app wants
  a swappable kernel connection. `EmbeddedClient::shutdown()` calls
  `Kernel::shutdown()`: it cancels every background job and waits, bounded —
  see "Teardown" below for the contract.

## Capability Features

The default build is deliberately minimal: real-file I/O and the
copy-on-write overlay, **no** process execution, host introspection,
desktop integration, or tokenizer. Each dangerous surface is a named opt-in
cargo feature on `kaish-kernel`:

| Feature | Gates | Default |
|---------|-------|---------|
| `localfs` | Real local filesystem: `LocalFs`, passthrough/sandboxed VFS modes, spill-to-disk | ✓ |
| `overlay` | Copy-on-write overlay FS (implies `localfs`) | ✓ |
| `subprocess` | External commands: exec/spawn/which/bg/fg/kill, PATH, signals, job control | — |
| `host` | Host introspection: `ps`, `uname --host`, `hostname` | — |
| `os-integration` | Freedesktop trash + XDG base directories | — |
| `tokens` | BPE tokenization (`tokens` builtin) | — |
| `full` | All of the above (`native` is an alias) | — |

Consequences for embedders:

- **External commands need `subprocess`.** Without it, PATH lookup and
  `exec`/`spawn` don't exist. With it, gate at runtime via
  `allow_external_commands` (see [Sandboxing](#sandboxing-and-external-commands)).
  Git is an ordinary external command (`git status`, `git log`): it runs via
  `subprocess` against your system `git`, with no in-tree builtin or backend.
  The old `kaish-tools-git` crate (git builtin + `GitVfs`, removed in 0.9.0) is
  being reinvented as a shallow, safety-first git plugin in
  [kaish-extras](https://github.com/tobert/kaish-extras) — history, autopsy,
  and design intent live in `docs/git.md` there, and that repo is its
  maintained home going forward.
- A read-only agent shell wants the default features plus a custom backend —
  see [with_backend hermeticity](#custom-backend-kernelwith_backend).

## Kernel Construction

### Modes (`KernelConfig`)

```rust
use kaish_kernel::{Kernel, KernelConfig, VfsMountMode};
use std::path::PathBuf;

// Throwaway kernel, sandboxed defaults
let kernel = Kernel::new(KernelConfig::transient())?;

// Named kernel sandboxed to a specific root
let config = KernelConfig::named("my-kernel")
    .with_vfs_mode(VfsMountMode::Sandboxed {
        root: Some(PathBuf::from("/custom/root")),
    })
    .with_cwd(PathBuf::from("/custom/root"));

// Full host filesystem (what the REPL uses)
let config = KernelConfig::repl();

// Memory-only VFS, external commands disabled (tests, untrusted scripts)
let config = KernelConfig::isolated();

// Sandboxed-to-$HOME with a 64 MiB in-memory VFS budget (the agent preset)
let config = KernelConfig::agent();
```

Other builders: `.with_approvals(bool)` / `.with_trash(bool)` (destructive-op
rails — see below), `.with_vfs_budget(bytes)` / `.without_vfs_budget()` (cap
in-memory VFS growth), `.with_skip_validation(bool)`, `.with_initial_vars(map)`
(below).

#### Destructive-op rails: inspecting and fulfilling an approval gate

With `.with_approvals(true)`, a destructive op (`rm`'s delete, and the truncating
overwrite behind `tee` / `patch` / `sed -i` / `write` / `cp` / `mv` / `dd of=`)
does not run on first call — it returns an `ExecResult` with **exit code 2** and a
pending approval request. Copying or moving *into* a directory, and recursive
`cp -r`/`mv` of a tree, gate only the named destination, not per-child overwrites.
`kaish-trash empty` gates **always**, whatever the option says. The output
contract:

- **`ExecResult.err`** (which a frontend routes to stderr) carries the
  human-readable message;
- **stdout** is empty (nothing happened, so there is no success output);
- **`ExecResult.approval`** carries the pending decision as a first-class
  typed field — `Option<Box<PendingApproval>>`, control-plane and distinct
  from the data-plane `.data`. `PendingApproval { request, resume }` pairs
  the tokenless view with the route that resumes it.

The view is the whole inspect contract, and it is **tokenless by
construction** — there is no credential field to redact, so it is safe to log,
serialize, and hand to a reviewing model:

```rust
pub struct ApprovalRequestView {
    pub id: RequestId,            // the request's public name
    pub operation: OperationId,   // "fs.remove", "fs.overwrite", "fs.rename", "trash.empty"
    pub risk: RiskClass,
    pub resources: Vec<Resource>, // what it would touch (kind + id, e.g. path)
    pub principal: Principal,     // who is asking
    pub capture: Capture,         // Exact(tool, argv) when replayable
    pub job_id: Option<u64>,      // set when a backgrounded job raised it
    pub reason: String,           // why the gate fired
    pub hint: String,             // human re-run string (display only)
    pub deadline: Option<SystemTime>, // yours to set; compared when observed,
                                      // never enforced on a timer
    // … see kaish_types::approval for the full record
}
```

**Authority is a capability, not a flag.** `Kernel::build(config)` returns
`(Kernel, ApproverHandle)` — building the kernel is the only way to obtain one,
and you decide which sessions get a clone. `Kernel::new` is `build` with the
handle dropped, which is the right posture for a session that must not approve
its own work.

Two doors install it *on a session*, which is what makes `approvals
grant`/`deny`/`revoke` work there:

- `KernelConfig::with_approver_handle(handle)` — for a second kernel, which
  also adopts that handle's ledger, so several kernels share one log.
- `KernelConfig::with_own_authority(true)` — for the embedder that is itself
  the operator, with one kernel and no earlier handle to adopt. The reference
  REPL uses this; `Kernel::build` still returns the handle, and a clone is left
  on the session.

Neither is the default. An agent session is built with neither and has no
method that grants.

**Inspect** with the typed accessor (works before or after `--json` —
`.approval` survives formatting):

```rust
if let Some(req) = result.approval_request() {
    // apply preapproval policy / model review over (req.operation, req.resources),
    // or inspect req.capture — the exact argv a confirm would replay …
}
```

**Fulfill** by granting through the handle, then replaying with
`Kernel::confirm`. The replay reserves the attempt *first* and dispatches the
captured invocation with that correlation, so the gate it re-enters matches its
fresh draft against the granted request instead of posting a second one:

```rust
let (kernel, authority) = Kernel::build(config)?;
let gated = kernel.execute("rm 'my notes.txt'").await?;
if let Some(req) = gated.approval_request() {
    if approve(&req) {                                    // your policy
        authority.grant(&req.id, req.revision, terms).await?;
        let done = kernel.confirm(&authority, &req.id).await?;  // replays exactly
    }
}
```

`req.revision` is the revision this same read of the request saw — quote it back
(spec §B.6). A grant, denial, or cancellation quoting anything else is refused as
`LedgerError::StaleRevision` and recorded as `RevisionRejected` rather than
applied: your view of the request has gone stale, because something else already
decided, cancelled, or superseded it since you read it. This is what makes
out-of-band approval safe across an unbounded gap — a human answering a prompt
for a request that was cancelled while they were thinking cannot revive it.

**The wait is yours, and that is deliberate.** `approve(&req)` above can return
immediately, or it can pop a dialog, call a model, or sit in a queue until someone
comes back from lunch — `execute` has already returned, so nothing in kaish is
holding a statement open while you decide. There is no approval callback for the
kernel to await, because awaiting one would mean the kernel bounding your work with
a timeout it picked and cancelling your in-flight dialog or RPC when that timeout
fired. Run the decision on your own task, with your own timeout if you want one,
and end an abandoned request with `cancel` rather than letting it sit live.

`ApprovalOutcome::Pending` carries a `PendingApproval { request, resume }`, and
`resume` is a structured `ResumeAction` rather than something to infer from the
capture's shape: `ConfirmStatement { plan_digest, index }` for a held statement,
`RetryOperation` for an invocation `Kernel::confirm` can replay, and
`NotReplayable { reason }` for a capture the kernel cannot re-issue at all.

**`ExecResult.approval` carries the whole `PendingApproval` — read the route,
don't rebuild it.** `ExecResult::pending_approval()` hands back the same
`{ request, resume }` pair `ApprovalOutcome::Pending` carried at the gate
site; `result.approval_request()` stays the narrower accessor for a caller
that only wants the view. Read `.resume` off what `pending_approval()`
returns rather than calling `ResumeAction::for_capture` yourself — that
function is what built the route the first time, and calling it again at the
read site is exactly the duplicate-derivation this pairing exists to avoid.
`PendingApproval` and `ResumeAction` are re-exported from
`kaish_kernel::ledger`; `ApprovalRequestView` is reached through
`PendingApproval::request` (or imported from `kaish_types::approval`).

Two consequences worth planning for:

- **A gate stops the program there — exit 2 halts the statement loop.** This
  holds whether the gate was raised on the statement itself or by an `fs.*`
  operation inside it: exit 2 means *this has not happened yet*, so the
  statements written after it do not run. `confirm` replays the held statement
  and nothing after it. If you submitted `a; b; c` and `b` gated, running `c` is
  yours to do — the session still holds the variables and cwd that `a` set, and
  `ResumeAction::ConfirmStatement`'s `index` says where to pick up (`index + 1`).
- **Decide the requests you are handed.** With no expiry, an undecided request holds
  a live slot until something closes it, so an embedder that ignores pending requests
  eventually fills the ledger (`live_capacity`). Read `ExecResult.approval` on every
  result, not only the ones you expected to gate.

**Persisting a pending decision across a restart.** `PendingApproval` and
`ResumeAction` derive `Serialize`/`Deserialize`, so an ACP-style embedder that
parks a decision outside the process — a queue, a database row, a chat
message waiting on a human's reply — does not have to lose it if the process
exits before the answer arrives:

```rust
let pending: PendingApproval = gated.pending_approval().expect("a gated result");
let json = serde_json::to_string(&pending)?;
queue.push(json); // survives a restart; `pending.request.id` names the request to grant later
```

The request the JSON names is still live in the ledger regardless of whether
the process that raised it restarted — the ledger is what persists the
chain, not this snapshot; `pending`/`resume` are what a *reader* needed to
show and re-drive it, not the record of it.

Prefer `confirm` over hand-building the re-run. The `hint` field is a
*human-display* string and does **not** robustly quote paths (`rm
--confirm="T" my notes.txt` re-parses as two paths); `confirm` replays the
captured argv through the argv door and sidesteps that entirely. A session
without a handle can still fulfill a gate by presenting a key it was given:
re-run with `--confirm=<token>`, retrieved by an authority via
`ApproverHandle::token_for`.

**A grant authorizes exactly one successful run.** A key presented after a
successful settlement reports the recorded outcome instead of running the
operation a second time. A *failed* attempt does not consume the grant, so a
transient failure retries inside the grant's lifetime. Repetition that is
genuinely wanted has a first-class form: a standing grant with a use count.

**A grant is re-checked against the world before it runs.** A request may
declare a `transition` on a resource — the state it expects to find. The
kernel's gated overwrite declares the target's `sha256` content digest; a
grant copies those claims into its `conditions`, and redemption reads the
resource again before reserving anything. A resource that moved in between
appends `Refused` + `Voided`, returns **exit 1** naming what changed, and the
operation does not run. This *detects* a stale authorization; it does not make
the write atomic, which only a conditional write at the resource itself can do.

Build a grant's terms with `GrantTerms::once_for_view(&view, not_after)` — it
carries the request's declared transitions into the grant's conditions. Terms
that drop one are rejected as widening (**exit 1**,
`LedgerError::ConditionsWidened`).

**A resource kind you introduce needs a resolver.** The kernel resolves `path`
through its own backend. A tool that names any other kind registers a
`StateResolver` for it:

```rust
#[async_trait]
impl StateResolver for GitRefResolver {
    fn kind(&self) -> &str { "git.ref" }
    async fn observe(&self, id: &str) -> Result<StateClaim, ResolverError> {
        Ok(StateClaim::Exact(self.repo.oid_of(id)?))  // Err refuses; never Ok(Unspecified)
    }
}

let config = KernelConfig::repl().with_state_resolver(Arc::new(GitRefResolver::new(repo)));
```

Three rules the mechanism does not bend on:

- A kind with **no registered resolver refuses** any grant that claims a prior
  state for it. Registering the resolver is part of shipping the kind.
- A resolver that returns `Err` **refuses**. There is no `Ok(Unspecified)`
  escape — an unobservable precondition is never a passing one.
- Registering a resolver for `path`, or two for one kind, fails
  `Kernel::build`. Which resolver decides whether a resource changed must not
  depend on registration order.

A transition of `StateClaim::Unspecified` claims nothing and is never checked,
so a resource that declares none (`Resource::plain`) costs no I/O at
redemption. `rm` uses that form deliberately: digesting an `rm -rf` tree would
cost a full read per path. The `Redeemed` entry records the observations, so an
empty observation set is the record that a grant was unconditioned.

The kernel owns the *mechanism* (posting the request, capturing the argv at the
dispatch seam, minting and checking the credential); the embedder owns the
*judgment*. The request is **never** folded into `.data` — a stdout redirect
(`rm big > log`) clears the data-plane `.data` but can't touch the
control-plane `.approval`, so the gate can't be silently bypassed.

> **Note:** the argv is captured at the kernel's dispatch seam, so it's present
> for every kaish builtin and any tool you register in the kernel's registry
> (the `Kernel::with_backend` tools closure). A tool served *only* by a custom
> `KernelBackend::call_tool` that raises its own gate records a non-`Exact`
> `Capture`; `confirm` then fails loud (exit 2) naming the variant it found,
> rather than replaying an empty argv. Those requests are still grantable and
> still redeemable by presenting the key with `--confirm=<token>`.

If you executed with `--json` (`OutputFormat::Json`), the gate is a non-zero exit
with a diagnostic, so the result is wrapped in the standard JSON error envelope
and the request is surfaced under its own `approval` key:
`{ "error": "...", "code": 2, "approval": { "id": ..., "operation": ...,
"resources": [...], "hint": ..., ... } }`. The typed `approval_request()`
accessor works the same either way, so it's the recommended path.

**A request nobody decides does not expire.** It stays `Requested` until you
decide it or cancel it — kaish never reads a clock to end one, because how long
an unanswered request should live differs per embedder and a kernel default is
silently wrong for someone. What bounds the ledger instead is capacity:
`live_capacity` refuses a new request with a number when the ledger is full,
which is a failure someone can act on rather than a request that quietly
vanished.

**So closing what you no longer want is yours to do.**
`Kernel::cancel_approval(&id, rev, why)` closes an undecided request. (It is not
spelled `cancel`: `Kernel::cancel` already means "interrupt the running
execution".) Four properties an embedder should know:

- **Cancellation takes no `ApproverHandle`.** It is a requester action: a
  session holding no authority cancels its own requests, which is what lets a
  gated agent withdraw an ask it has given up on. Cancelling another
  principal's request without authority exits 1, naming both principals.
  `approvals cancel <id>` is the same call from the shell.
- **Only an undecided request is cancellable.** A decision that already landed
  is not undone by the requester losing interest — cancelling a granted request
  exits 1 — and a granted-but-unredeemed chain closes on its own at the grant's
  `not_after`.
- **`rev` is revision-checked, the same as `grant`/`deny` (§B.6).** Quote the
  revision your view of the request was at; a stale quote is refused as
  `LedgerError::StaleRevision` rather than applied. This is what keeps a
  deadline timer from cancelling a request a human just decided out from under
  it — whichever transition commits first invalidates the other's quote.
- **Asking again is a new request, not a revival.** Re-run the command; the
  fresh request starts undecided and is linked to the closed one by
  `supersedes`, so the whole thread of intent stays walkable.

**A deadline is a timer you run, not one the kernel runs.** Set
`RequestOrigin::with_deadline(Some(when))` if you want the request to *record*
one — it is compared when the request is next observed — and run your own timer
that calls `cancel_approval(&id, rev, CancelReason::DeadlinePassed)` when it
fires. An embedder that wants no horizon never calls it.

**And the clock those bounds are read against is yours too.**
`KernelConfig::with_approval_clock(Arc<dyn Clock>)` installs it; the default is
`SystemClock`. The kernel keeps no opinion about which clock is true — it keeps
two properties:

- **One clock per ledger.** The reading an entry is stamped with and the reading
  a bound is compared against come from the same source, so a record's
  timestamps and the decisions taken alongside them can never mean two different
  clocks. `Ledger::build` takes the clock as a required argument for exactly
  that reason.
- **A monotone non-decreasing view of it.** The ledger latches the largest
  reading it has taken and clamps a smaller one up to that latch, so an expired
  grant stays expired and entry stamps never regress — whatever your clock does.
  You do not have to promise the kernel a well-behaved clock.

This is the *ledger's* clock, not the kernel's: `timeout`, the script watchdog,
and `ToolCtx::patient` run on `Instant` and are unaffected. It is also
incompatible with `with_approver_handle`, which adopts a ledger that already has
one — setting both fails `Kernel::build` loudly.

**kaish closes what its own teardown would strand.** `kill --discard %N`,
`Kernel::cancel_all_jobs`, and `Kernel::shutdown` cancel the requests they would
otherwise orphan — the last sweeping every live request in that kernel's scope,
which matters when several kernels share one ledger through
`with_approver_handle`. What kaish cannot close is a session that goes away
without calling `shutdown`; page through `Approvals::pending(PageRequest::default())`
and cancel what you recognize.

**Gated backgrounded jobs surface on the job, not on the result.** `cmd &` that
gates inside tool execution puts the request on `JobInfo.approval` and
`/v/jobs/{id}/approval`; the statement that spawned it has already returned. An
embedder that reads only `ExecResult.approval` will not see it. Poll
`jobs --json` for gated entries, or page `Approvals::pending(..)`, which is
the authoritative set either way.

**Reading the ledger.** `Kernel::approvals()` is the read side (it grants
nothing): `pending(PageRequest)`, `ids()`, `get(&id)`, `standing()`,
`subscriptions()`, `any_subscriptions()`, `log(since, limit)`, and `watch(since)`.

`pending`/`log` return a **page**, not a bare `Vec` — `ApprovalPage`/`LedgerPage`,
each `{ items, next: Option<LedgerCursor> }`. The statement tap posts an
`Observed` entry per top-level statement, so both sets are unbounded in
principle; a `next` cursor means more remain. Resume with it —
`PageRequest::default().with_cursor(next)` for `pending`, `next.seq()` as the
next call's `since` for `log` — and a reader that stops and resumes this way
never misses an entry or sees one twice. `PageRequest::default()`'s limit
(1024) already covers a realistic pending set in one page; reach for the
builder methods (`.with_limit`, `.with_scope`, `.with_state`, `.with_since`)
to filter or to page deliberately.

`log(since, limit)` returns `LedgerRecord`s, never bare entries: each carries
`schema_version`, `sequence`, `at`, and the `scope` the entry belongs to,
alongside the entry itself. Read `record.known()` for the entry — `None` means
a **newer writer's entry this build does not recognize**, kept verbatim as
`RecordedEntry::Unknown` with its sequence and scope intact. Surface it as
unknown; never drop it, or the history you report is one you did not verify.
`record.schema_is_known()` answers the same question for the envelope.
`LedgerSink::post` receives the same `LedgerRecord`.

**Watching instead of polling.** `Approvals::watch(since)` is the one
convenience the kernel offers around waiting: it backfills the retained tail
from `since`, then yields every further append live, in the same order,
until the stream is dropped. There is no deadline argument and no filter —
those are policy an embedder builds on top, not something the kernel decides
for it (§0.1's line: mechanism, not policy).

```rust
let mut stream = kernel.approvals().watch(0);
loop {
    match stream.next().await {
        Some(WatchEvent::Entry(record)) => {
            // react to `record.known()` — a Requested you should surface to
            // a human, a Granted that unblocks a queue, whatever this
            // embedder's own event loop wants.
        }
        Some(WatchEvent::Lagged { count }) => {
            // this consumer fell behind the broadcast buffer — `count`
            // entries were dropped, never silently. Catch up before
            // resuming the live tail:
            let page = kernel.approvals().log(last_seen_seq, 4096);
            // .. process page.items, remember the new last_seen_seq ..
        }
        None => break, // the ledger itself is gone — process shutdown
    }
}
```

A lagging consumer is reported, not dropped silently — `WatchEvent::Lagged`
names how many entries this stream's subscriber missed, so the recovery
above is `Approvals::log` from the last `seq` this stream actually delivered,
not a guess.

The same read model is projected at **`/v/approvals`** — `pending`,
`standing`, and `log` at the root, and `{id}/{request,state,attempts,grant}`
per request — and surfaced by the `approvals` builtin (`list`, `show`,
`log`). `/v/approvals` is **read-only: every write path returns
`Unsupported`**, because granting by file write would make "the agent can
write files" equivalent to "the agent can approve its own operations". No
projection carries a credential; there is no redaction step, because no
projected type has a credential field.

**The reference REPL is a worked example of all of the above**, and it is a
plain embedder with no privileged hook: `crates/kaish-repl/src/approval.rs`
plus `Repl::fulfill_gate`/`decide` in `lib.rs` are the whole of it — get
`Pending` back from `execute`, render it, read one answer, `grant_with_grounds`
+ `Kernel::confirm`, or `deny`. Copy its two rules whatever your frontend is:

- **An approval prompt is never the agent's output stream.** This is a
  requirement, not a preference: a question a model can read as data is a
  question it can answer, and an approval surface a model can reach is not an
  approval surface. The REPL renders the request to **stderr** and asks only
  when **stdin and stdout are both terminals**; a piped or captured session
  gets the exit-2 result and no question at all. Segregate the prompt the same
  way in whatever you build — a separate pty, a UI channel, a socket the model's
  side cannot write.

  **Put the terminal check beside the write, not at the construction site.**
  `TerminalPrompt::ask` calls `IsTerminal` on the line immediately above
  `eprint!`, so the rule lives where the output happens and there is no
  configuration path that can construct a prompt that asks into a pipe. A check
  made once at startup is a check that stops being true when a frontend is
  reused, re-parented, or handed a different stream, and it fails open.
- **Anything that is not an answer denies.** `n`, an empty line, Ctrl-C,
  Ctrl-D, a terminal that went away: all of them close the request. A gate left
  live because nobody could be asked is a slot nothing will ever return.

**Deciding from inside a session.** `KernelConfig::with_approver_handle()` (or
`with_own_authority(true)` for a single-kernel embedder) installs an authority
*on the session*, which is what lets `approvals grant`/`deny`/`revoke` work
there. Without one those three exit **1** naming
the reason, while `list`, `show`, `log`, and `cancel` keep working. That is
the whole separation: an agent that can run any shell command can see what is
pending and withdraw its own requests, and cannot approve itself.

To decide a request raised in one `execute()` call from
a *later* call — or from a different kernel — share the ledger with
`KernelConfig::with_approver_handle()`; the default is a fresh ledger per
kernel. `KernelConfig::with_ledger(LedgerConfig::default().with_…())` tunes
capacity, retention, the sink queue, and the rejected-credential limit;
`LedgerConfig` is `#[non_exhaustive]`, so use the builder rather than a struct
literal.

**The audit sink, and what a slow or failing one costs you.**
`KernelConfig::with_ledger_sink(Arc<dyn LedgerSink>)` posts every `LedgerRecord`
to your sink as it commits, in commit order, from a background drain task. Your
`post` must be fast and non-blocking: it always has a queue slot reserved for it
already, so it never negotiates capacity with the ledger and only ever does its
own I/O.

```rust
pub trait LedgerSink: Send + Sync {
    fn post(&self, record: &LedgerRecord) -> Result<(), LedgerSinkError>;
}
```

Four rules, each of which will bite an embedder that writes to a network log:

- **A full queue refuses new privileged operations — exit 1.** The queue is
  `LedgerConfig::sink_queue` deep (default **1024**). Once every permit is
  reserved, the next `post_request`/`grant`/`deny` fails with
  `LedgerError::SinkUnavailable`, which reaches the gate site as
  `ApprovalOutcome::LedgerUnavailable` and exits **1**. The ledger never blocks
  the executor and never drops an audit record to make room.
- **A terminal entry is never refused.** `Settled`, and attempt-level
  `Abandoned`, have their slot banked with the `Redeemed` entry that opened the
  attempt, before the attempt is allowed to begin — an operation that already
  ran can always record what happened, whatever the queue is doing.
- **An `Err` trips the sink permanently. There is no retry.** The drain task
  stops consuming, and the failed record plus everything queued behind it is
  counted as undelivered and named in every later refusal: `"audit sink failed;
  N audit entries undelivered — refusing further privileged operations until the
  process is restarted"`. The loss is accounted, never silent. Recovery is a
  process restart; there is no reset call, deliberately, because an unrecorded
  privileged operation is exactly the corruption this design refuses.
- **Buffering is your call to make explicitly.** A sink fronting something that
  can be unavailable — a network collector, a remote audit service — should
  buffer internally and return `Ok` quickly, accepting the buffering risk in
  your own code. The kernel will not silently trade a complete record for
  availability on your behalf.

The sink is an **export, not a source of truth**. It is post-only: nothing reads
back through it, so a restart does not reconstruct prior chains from sink output
(`docs/approval-ledger.md` §B.1 — v1 is in-process only, with no durability
claim). What recovery there is happens in-process: a periodic sweep closes
attempts whose guard was dropped without draining, as `Abandoned`.

**Hosting several sessions in one process.** Every request carries an
`ApprovalScope` — a kernel id, an optional session, and an optional actor — so
"whose request is this?" is answerable from the request itself rather than from
a map you keep beside it. Name the session with
`KernelConfig::with_session(SessionId::new("conversation-7"))`; a kernel with
none is the single-session shape.

`Approvals::scope(session)` and `ApproverHandle::scope(session)` derive views
restricted to one session. The scoped read side sees only that session's
requests and records, and the scoped authority decides only within it —
`grant`, `deny`, and `token_for` on another session's request return
`LedgerError::OutOfScope`. **Scope the read side too**: the statement tap puts
the command text that raised a request into the request, so an unscoped reader
in a multi-session process reads every session's commands. A request with no
session belongs to the kernel and is invisible to every scoped handle.

This is API hygiene, not a process boundary. It stops a session's code from
reaching another session's requests by accident or by confusion; it does not
stop hostile Rust in the same process, which can reach anything the process
can.

**A grant is a decision about an operation in a context.** Every request
records the `PlanBinding` it was judged under: a digest of what was judged, the
working directory, the scope, and the sandbox profile when you name one. A
`--confirm=<token>` presented — or a `Kernel::confirm` replay dispatched — from
outside that binding **redeems nothing**. The kernel posts a fresh request and
returns exit 2 with it, rather than authorizing an operation nobody judged in
that context; the grant it did not redeem stays live and untouched. The
common case is a changed working directory, which no precondition resolver can
catch, because nothing declares the cwd as a precondition.

**Recording without gating: `fs.*` subscriptions.** `set -o approvals` is one
posture — enforce, over the whole `fs.*` namespace. A subscription generalizes
it: `ApproverHandle::subscribe(Subscription::new(operations, resources, mode,
reason))` registers a glob over (operation, resource) in one of two modes.

| Mode | What matching operations do |
|---|---|
| `SubscriptionMode::Observe` | Post one `LedgerEntry::Observed` and **run**. Never defers, never blocks, never returns exit 2. A record, not a decision — no request is built, no grant exists, nothing enters the live index, and the entry is evictable the moment it commits. |
| `SubscriptionMode::Enforce` | Go through the full decision chain, exactly as `set -o approvals` does — but scoped to the glob instead of the whole namespace. |

Operations glob (`fs.*` covers the namespace); resource **kinds match exactly
and only the `id` globs**, so `ResourcePattern::new("path", "/workspace/**")`
never matches a `git.ref`. An empty pattern list matches nothing rather than
everything. `subscribe` returns a `SubscriptionId`; `unsubscribe(&id, reason)`
revokes it, and both append their own ledger entry (`Subscribed`,
`Unsubscribed`) — an audit scope that changed with no record of the change
makes the record it produced unreadable. Revocation takes effect for
operations not yet posted; a request already granted under the subscription is
unaffected.

Three rules worth knowing before writing a glob:

- **Unsubscribed and ungated means unposted.** An `fs.*` operation nothing
  covers posts **nothing at all** — no `Requested`, no allocation. The gate
  sites take one relaxed atomic load (`Approvals::any_subscriptions()`) before
  building anything, so a 10,000-path delete on a session with no subscription
  costs one branch. `ApprovalRequest::constructed_count()` is the counter that
  proves it: read it either side of a command and the difference is **0**.
- **`enforce` beats `observe`** when both cover a path. Enforce is strictly
  stronger and its record is a superset, so the other precedence could
  downgrade a gate to a note.
- **Matching is per resource, and against the resolved path.** `rm
  /workspace/a /tmp/b` under an observe subscription on `/workspace/**`
  records `/workspace/a` and stays silent about `/tmp/b`. The glob is matched
  against the path the kernel resolved — so a relative path cannot step
  outside the scope — and each recorded `ObservedResource` carries both
  spellings: `id` is the path the command wrote (what an auditor recognizes),
  `resolved` is the path the glob matched, and `subscription` names the
  covering subscription. One command's covered paths post as one entry, even
  when their coverage comes from different subscriptions.

`trash.empty` gates regardless of any subscription: it discards the recovery
net every other `fs.*` operation relies on. `Approvals::subscriptions()` lists
the live registry; there is no shell surface for `subscribe` — a session that
could subscribe itself could also unsubscribe itself.

**The statement tap: every statement is recorded, always.** Above the `fs.*`
layer sits a second one. Every top-level statement posts one chainless
`LedgerEntry::Observed` entry under operation `cmd.execute`, carrying a `Plan`
— the statement rendered back to shell text unexpanded, its kind, and every
command it contains. **There is no configuration that turns this off**: an
agent getting an automatic second opinion is a property of kaish, not a
posture an embedder selects. The two layers are independent by design and join
through trace context, not through the kernel: `fs.*` records what a command
**touched**, `cmd.execute` records what was **asked to run**, before any of it
runs.

The cost is one entry per top-level statement. A 1,000-iteration loop posts
one; a 10,000-path `rm` posts one; a `$(…)`, a sourced script, and a user
tool's body post none of their own, because their statements belong to the
enclosing top-level statement's plan.

**So every kernel's log accumulates, including one with approvals off** —
"approvals off" means nothing gates, not that nothing is observed, and a
kernel that never raises a request still records a `cmd.execute` entry per
statement (the `cat` that reads the log included). Growth is bounded at
`LedgerConfig::retained_entries`, **4096** entries by default and settable with
`with_retained_entries`; a chainless observation always evicts freely, so the
oldest ones age out rather than filling the ledger. Read them with `approvals
log --limit N` (200 by default, `--since SEQ` to page) or `cat
/v/approvals/log`.

**The tap is advisory, not a durable audit trail.** A tap append that cannot
commit — sink backpressure, a full retention ring — emits a **warn** event and
the statement **still runs**. Nobody opted into a completeness guarantee here;
an operator who registers an `fs.*` subscription did, which is why that path
exits 1 and this one does not. An embedder that needs the record to be
complete gets that from the sink's own reliability (`with_ledger_sink`), not
from the tap. A gate-classified statement keeps every fail-closed rule: a
decision that cannot be recorded is not made, and the statement exits **1**.

**The classifier scopes; the chain decides.**
`KernelConfig::with_statement_classifier(Arc<dyn StatementClassifier>)`
installs one. It is called once per top-level statement, synchronously, and
must not block — it runs on the execution path of every statement.

```rust
use kaish_kernel::ledger::{
    ClassificationError, ExecutionContext, StatementAssessment, StatementClassificationInput,
    StatementClassifier, StatementPosture,
};
use kaish_types::approval::{AssessorId, RiskClass};

struct GateWrites;
impl StatementClassifier for GateWrites {
    fn classify(
        &self,
        input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        let posture = if input.plan.commands.iter().any(|c| !c.redirects.is_empty()) {
            StatementPosture::gate(
                "the statement redirects to a file",
                RiskClass::Recoverable,
            )
        } else {
            StatementPosture::Observe
        };
        Ok(StatementAssessment::new(posture, AssessorId::new("gate-writes")))
    }
}
```

With none registered every statement is `StatementPosture::Observe` — recorded
and run. A `StatementPosture::Gate { reason, risk }` builds an
`ApprovalRequest` under `cmd.execute` with the plan attached and one
`Resource { kind: "cmd", id: <argv0> }` per planned command, then runs the same
three-stage chain a gated `rm` runs: a standing grant auto-approves it
(all-or-nothing over every `cmd` resource), `Policy::evaluate` may grant or deny
it on the request path, and `Defer` through both is **exit 2** with the view
on `.approval` and **nothing of the statement executed** — no substitution, no
redirect target created, no first loop iteration, and no statement after it in
the same program. The classifier has no deny:
refusal is a chain decision (`Policy::evaluate`), because a scoping seam that
can refuse is a second decision chain.

**`Err` means `Gate`, never `Observe` — and a panic means the same thing.**
`classify` returns `Result<StatementAssessment, ClassificationError>`. An
`Err` — the model backing a classifier is unreachable, the input is outside
what it can judge, anything it cannot answer — maps to `Gate`, never to a
silent `Observe`: a classifier that cannot answer must not be able to turn
the statement gate off. A panic inside `classify` is caught (`catch_unwind`
at the tap site) and mapped the same way, so a bug in an embedder-authored
classifier gates the one statement it broke on rather than crashing the rest
of the program. This is a **looser** contract than `Policy::evaluate`'s,
which still propagates a panic unguarded — `evaluate` only runs once a
decision is genuinely being asked for, while a classifier runs in front of
*every* statement, including the ones nobody would ever gate. A classifier
may also **raise** the posture a kernel-owned static rule already set, but
never **lower** it — `kaish-trash empty` gates even under a classifier that
always answers `Observe`.

**`ExecutionContext` carries no host path.** `StatementClassificationInput`
pairs the plan with an `ExecutionContext { cwd, scope, sandbox_profile,
mounts }`. `cwd` is a `String` holding the logical VFS path — the spelling
the router resolves against, the same convention `PlanBinding::cwd` uses —
never a raw host `PathBuf`. A classifier is frequently a call into a model,
and that input frequently leaves the process; `/home/amy/clients/acme` says
things a `MountClass::Project` does not.

`CommandNameClassifier::new(names, reason, risk)` is the reference
implementation: it gates a statement when any command it plans is named in
`names`, matched on argv0 exactly as written. Classifying the plan is what
tells `rm target.txt` from `echo 'rm target.txt'` and from `grep rm
changelog.txt` — the plan carries the parse, and a raw line does not.

**Every classifier judgment that leads to a `Gate` is recorded as an
`Assessed` entry** (`docs/approval-ledger.md` §C.7) — `assessor`, `stage:
Classifier`, `outcome`, `reason`, and the model identity/confidence a
`StatementAssessment` carried, once the request it explains exists. An
embedder's own deliberation pipeline appends more of the same shape through
`ApproverHandle::assessments()`, so `Approvals::get(id).assessments` reads
the whole chain of reasoning that led to a decision — or that never reached
one, when the request was abandoned instead.

**Replay of a held statement.** A deferred statement's capture is
`Capture::Statement { source, index }`: statements carry no source spans, so
the capture is the program source plus the held statement's index.
`Kernel::confirm(&handle, &id)` re-parses that source and runs exactly that
statement, in the originating session — earlier statements' effects
(variables, cwd) are session state and still hold, and they are not re-run. A
gated `execute_argv` call captures `Capture::Exact` instead, because it
already holds a tool name and an argv.

**Redeeming a held statement by re-running it.** The statement gate reads a
`--confirm=<key>` off the statement's own argv before it drafts, so re-running
the held line with the key an operator hands back redeems **the original
request** rather than minting a second one. The same pass keeps the credential
out of the record: the rendering shows `--confirm=<confirm-key>` and the
captured source drops the token entirely, because plan and capture both land
in the ledger and no ledger entry carries a credential. What *executes* is
untouched — the builtin's own gate may legitimately consume the same key.

Only a literal key is visible to any of this. `--confirm=${key}` renders
unexpanded, so nothing is lifted and nothing needs redacting: what the plan
cannot see, it cannot leak either. A credential a script puts somewhere the
taxonomy cannot name — the right-hand side of an assignment — is recorded like
any other text. The kernel redacts what it minted; everything else is the
`Redactor` seam below.

**Installing a `Redactor` for everything else.** The kernel's own redaction
above covers exactly one string — its own confirm key — because it is the
only secret the kernel can identify without guessing. Every other value in a
`Plan` (`PlannedCommand::args`, `PlannedRedirect::target`) reaches the
statement classifier, the ledger's `Observed` entry, and the `/v/approvals`
projection as `PlannedValue::Plain` unless an embedder installs a `Redactor`
that says otherwise:

```rust
use kaish_kernel::ledger::{RedactionMark, Redactor};
use kaish_types::approval::ValueSite;

struct BearerTokens;
impl Redactor for BearerTokens {
    fn redact(&self, value: &str, _site: ValueSite) -> Option<RedactionMark> {
        value.starts_with("Bearer ").then(|| RedactionMark::new("bearer-token"))
        // .with_fingerprint(digest) if you want an auditor to be able to
        // ask "the same credential as last time?" without holding it.
    }
}

let config = KernelConfig::repl().with_redactor(Arc::new(BearerTokens));
```

It runs once, synchronously — like `StatementClassifier::classify`, on the
execution path of every statement, so it must not block — at the one
normalization point before the plan reaches any sink, so a sink added later
inherits the redaction instead of needing its own fix. It is not consulted
on `--confirm=<key>`: that redaction is unconditional and happens first, and
`kind: "confirm-key"` is reserved for it (a `Redactor` returning that string
for something else just means the kind label collides in the record, not a
security hole). `Capture::Statement`'s replay source is **not** covered by an
installed `Redactor` — `Kernel::confirm` re-executes it verbatim, so a
redacted value baked into it would replay as the literal marker instead of
the argument you meant. If a `Redactor`-marked value can appear on a
statement that gets held, it is still visible in the request's `capture`
until the request is granted or denied — narrow the exposure with a tighter
`StatementClassifier` (gate before the value would be typed) rather than
expecting the `Redactor` to close it.

**Pinning the policy.** `KernelConfig::with_policy_pinned(true)` makes
`set +o approvals` fail with **exit 1** and a message naming the pin, rather
than silently doing nothing — a silent no-op would teach an agent that its
`set +o approvals` worked. The pin is copied into every fork and pipeline
stage, and survives `Kernel::reset`.

See [LANGUAGE.md](LANGUAGE.md) for the full approvals/trash semantics.

### Custom Backend (`Kernel::with_backend`)

For full control over file I/O, implement `KernelBackend` (from
`kaish-tool-api`, re-exported by the kernel) and assemble with:

```rust
let kernel = Kernel::with_backend(
    backend,            // Arc<dyn KernelBackend>
    config,             // KernelConfig
    |vfs| {             // mount extra filesystems
        // vfs.mount_arc("/v/docs", docs_fs);
    },
    |tools| {           // register custom tools
        // tools.register(MyTool { ... });
    },
)?;
```

> **Warning:** `with_backend` kernels are **hermetic by construction**:
> kaish mounts no host filesystem (your backend is the only I/O path),
> output spill is forced in-memory (no host temp files), and
> background-job output files are disabled. If your embedder previously
> relied on disk spill or `/v/jobs` persistence, that data now stays in
> memory.

`with_backend` also mounts `/dev` (`DevFs`: `/dev/null`, `/dev/zero`,
`/dev/random`, `/dev/urandom`) unconditionally, kernel-owned, alongside
`/v/jobs` and `/v/blobs` — this holds even if your own backend is read-only,
so `cmd > /dev/null` always discards rather than failing as a filesystem
error.

A `with_backend` kernel owns its VFS, so `KernelConfig::with_vfs_budget`
does not see your mounts — cap them yourself by constructing the backing
`MemoryFs` with `MemoryFs::with_budget(Arc<ByteBudget>)`. Both types are
available through `kaish_kernel::vfs`; no direct `kaish-vfs` dependency
needed:

```rust
use kaish_kernel::vfs::{ByteBudget, MemoryFs};

let budget = Arc::new(ByteBudget::labeled(16 * 1024 * 1024, "scratch"));
vfs.mount("/", MemoryFs::with_budget(budget.clone()));
// budget.used() / budget.remaining() are observable at any time.
```

### Output Limits and Spill Mode (`OutputLimitConfig`)

`KernelConfig::output_limit` caps how much a single command's output can grow
before it's truncated (exit code 3 — see [the result contract](#the-result-contract)).
Independent of the byte cap, `SpillMode` decides *where* the overflow goes:

- **`SpillMode::Disk`** (the default): the full output is written to a spill
  file under `paths::spill_dir()` — `$XDG_RUNTIME_DIR/kaish/spill` (tmpfs on
  systemd systems, cleared on reboot) — and the result carries a head+tail
  preview pointing at it (`cat` it to read the rest).
- **`SpillMode::Memory`**: head+tail truncation only — no disk I/O, no
  recoverable file. Memory stays bounded regardless of how much the command
  produced.

| Construction | `SpillMode` |
|---|---|
| `KernelConfig::agent()` / `.agent_with_root()` / `.named()` / `.transient()` (`Sandboxed`, real host mount) | `Disk` |
| `KernelConfig::repl()` (`Passthrough`, real host mount) | `Disk` in principle, but moot — `repl()`'s `output_limit` is `none()` (unlimited) |
| `KernelConfig::isolated()`, or any config `.with_vfs_mode(VfsMountMode::NoLocal)` | `Memory` — forced at construction, no host mount to spill to |
| `Kernel::with_backend(..)` | `Memory` — forced at construction, the embedder owns the VFS and a kernel-side `std::fs` write would bypass it (see the Warning above) |

Forcing beats an explicit request: setting `SpillMode::Disk` on a config that's
`NoLocal` or headed for `with_backend` is silently overridden to `Memory` in
`Kernel::assemble` — neither kernel shape owns a host mount to write to, so an
explicit `Disk` request there would be nonsensical, not honored.

A **host-backed** kernel (`Sandboxed`/`Passthrough`, built with `Kernel::new`)
defaults to `Disk` because it already has a real filesystem — spilling there is
no different from any other write it does. If you want a host-backed kernel
that nonetheless never touches disk (e.g. the output may hold data you don't
want recoverable from a temp file even though the kernel has host access),
opt in explicitly:

```rust
use kaish_kernel::OutputLimitConfig;

let config = KernelConfig::agent()
    .with_output_limit(OutputLimitConfig::agent().in_memory());
```

There's no equivalent flag to force `Disk` on a `NoLocal`/`with_backend`
kernel — by design, since neither owns a host mount to spill to.

> **v0.13.0:** the public `output_limit::spill_aware_collect` function (and its
> private helpers) is removed — it was dead since external-process capture
> moved onto `BoundedStream`/`drain_to_stream`, with spill applied post-hoc at
> the pipeline level (`Kernel::execute_pipeline` → `spill_if_needed`, both
> internal) instead of inline during capture. `OutputLimitConfig` and the
> disk/memory spill behavior documented above are unaffected; there was no
> embedder-facing replacement to migrate to because the function was never a
> supported extension point, just a capture helper that happened to be `pub`.

## Initial Variables and Hermetic Subprocess Env

The kernel is **hermetic by default** — it never reads `std::env::vars()`,
and external commands launched from inside the kernel see only the
variables kaish has marked as exported.

> **One exception, and it only ever turns a rail *on*.** Four `KernelConfig`
> presets read `KAISH_APPROVALS` and `KAISH_TRASH` from the process
> environment at construction (`repl()` and the agent presets — the ones a
> frontend uses). Nothing else in the kernel touches `std::env`, and the
> direction is safe: env can enable the approval gate or the trash, never
> disable one an embedder asked for. The right long-term shape is for the
> *frontend* to read env and pass `KernelConfig`; until then, an embedder that
> needs a guaranteed-hermetic construction builds its own `KernelConfig`
> rather than starting from a preset. Frontends that want shell-like UX
(the bundled REPL, or an embedder that mirrors the host shell) opt in to
OS-env passthrough by populating `initial_vars`:

```rust
use kaish_kernel::ast::Value;
use std::collections::HashMap;

// Bare embedder kernel: hermetic. Subprocesses see no PATH, HOME, etc.
let kernel = Kernel::new(KernelConfig::named("isolated"))?;

// Embedder that wants its own curated env:
let mut vars = HashMap::new();
vars.insert("PATH".to_string(), Value::String("/usr/bin:/bin".into()));
vars.insert("LANG".to_string(), Value::String("C.UTF-8".into()));
let kernel = Kernel::new(
    KernelConfig::named("curated").with_initial_vars(vars),
)?;

// Shell-like passthrough (what kaish-repl does):
let env: HashMap<String, Value> = std::env::vars()
    .map(|(k, v)| (k, Value::String(v)))
    .collect();
let kernel = Kernel::new(KernelConfig::repl().with_initial_vars(env))?;
```

Builders:

- `with_var(name, value)` — add a single entry
- `with_vars(map)` — extend the existing map (last write wins)
- `with_initial_vars(map)` — replace the entire map

All entries are marked exported when the kernel boots, so they reach
external subprocesses (`printenv`, `cargo`, `git`, …) directly. For
*per-call* variables, use `ExecuteOptions::with_vars` (next section)
instead of mutating kernel state.

## Per-Call Execution: `ExecuteOptions`

`Kernel::execute_with_options` is the canonical per-call surface:

```rust
use kaish_kernel::ExecuteOptions;
use std::time::Duration;

let result = kernel.execute_with_options(
    "build-report $REQUEST_ID",
    ExecuteOptions::new()
        .with_vars(request_vars)                 // function-local overlay
        .with_timeout(Duration::from_secs(30))   // per-call deadline
        .with_cwd("/mnt/repos/kaish".into()),    // per-call working dir
).await?;
```

> **Note:** `ExecuteOptions::with_vars` replaces `Kernel::execute_with_vars`,
> which is **deprecated**.

Fields:

- **`vars`** — per-invocation variable overlay with bash function-local
  semantics: a scope frame is pushed, each var set and marked exported
  (visible to the script and any subprocesses it spawns), and the frame is
  popped on return — inner assignments vanish with it, outer values and
  export bits are restored.
- **`timeout`** — per-call deadline; on expiry the result has exit code
  124. `Some(Duration::ZERO)` is a dry-run: validate and return 124
  without executing. A custom tool that legitimately outlives this
  deadline (a provider call that runs minutes) can suspend it with
  `ctx.patient(budget)` — see [Patient tools](#patient-tools-suspending-the-script-timeout).
- **`cancel_token`** — an embedder-owned
  `tokio_util::sync::CancellationToken`, *raced* against the kernel's
  internal token for the duration of the call (not stored). Cancellation
  cascades to forks and external children (SIGTERM → grace → SIGKILL on
  the process group).
- **`interrupt`** — `with_interrupt(Arc<dyn Fn() -> bool + Send + Sync>)`, a
  polled interrupt check for embedders whose thread can't fire `cancel_token`
  while execution runs — the motivating case is `wasm32-unknown-unknown`:
  single-threaded, so the page's main thread can only flip a
  `SharedArrayBuffer` flag for a Web Worker to poll, never cancel a token from
  outside. The kernel checks the closure at its existing cancellation
  checkpoints; a firing check takes the same exit-130 path as
  `Kernel::cancel()`/`cancel_token`, and session state survives. Scoped to the
  one call and cleared on every exit path — prefer `cancel_token` when your
  embedder's threading model allows it.
- **`cwd`** — per-call working directory override.
- **`stdin`** — standard input for this call as a ready, bytes-typed buffer
  (`impl Into<Vec<u8>>` — a `&str`/`String` or a raw `Vec<u8>` both work),
  consumed by the first top-level command that reads stdin (shell draining
  semantics — a later reader sees nothing). Lets an embedder feed piped input,
  e.g. `printf '…' | kaish -c 'sort'`, binary included — a byte-aware builtin
  (`wc -c`, `cat`, `cmp`, …) sees it intact, while a text-only builtin still
  refuses non-UTF-8 loudly when it asks for text. A redirect (`< file`/heredoc)
  on the command still takes precedence. Eager: the whole buffer must exist
  before the call. For a **lazy** stream — fed only if a command reads stdin,
  so an open process stdin that never sends EOF doesn't block a command that
  never reads it — use `Kernel::execute_with_pipe_stdin(_streaming)` with a
  `PipeReader` instead (this is how the non-interactive `kaish` CLI forwards its
  own process stdin, e.g. `sleep 10 | kaish -c 'echo hi'`). See
  [docs/binary-data.md](binary-data.md) for the full text-vs-bytes design
  behind this (`Value::Bytes`, `read_stdin_to_text` vs `_bytes`, which
  builtins are binary-aware).
- **`traceparent` / `tracestate` / `baggage`** — W3C trace context;
  kaish's execution span parents onto your trace, and baggage merges back
  out through `ExecResult.baggage`.

### Neither `timeout` nor `cancel_token` bounds a background (`&`) job

`ExecuteOptions::timeout` and `cancel_token` govern the call that started a
`&` job, not the job itself (GH #245). `cmd &` returns `[1]` the instant it's
registered — by the time the deadline or cancellation could fire, the call
that would have been bounded by it has already returned. The backgrounded
pipeline runs on its own fork with its own independent
`tokio_util::sync::CancellationToken`, deliberately detached so it survives
the parent call's cancellation (correct shell semantics — `&` is meant to
outlive the command that started it). `Kernel::cancel()` inherits the same
boundary: it cancels the *current* foreground execution, never a
backgrounded one. The only levers that reach a running `&` job are `kill
%N` from a script, or [`Kernel::cancel_all_jobs`]/[`Kernel::shutdown`] from
the embedder. There is no per-job timeout at all unless the script sets one
itself: `timeout 600 cmd &`.

### `Kernel::reset()` does not touch background jobs

`reset()` clears scope and cwd; jobs are not session state, so a `&`
started before `reset()` keeps running, stays in `jobs`, and the job ID
counter keeps counting up (GH #245). An embedder that treats `reset()` as
"new session" — a fresh MCP conversation reusing one kernel, say — inherits
every job the previous conversation backgrounded. Call
[`Kernel::cancel_all_jobs`] first if that inheritance is not wanted.

### The tokio runtime must outlive any background job you start

`execute_background` spawns onto whatever tokio runtime is current when
`execute()` runs (a bare `tokio::spawn`) — kaish does not capture or manage
its own `Handle` (GH #247). An embedder that builds a short-lived runtime per
call (`Runtime::new()` + `block_on` per request — a common pattern for a
one-request-per-thread server) has every background job it started in that
call die, silently, the instant the runtime drops mid-execution. `Kernel` is
`Send + Sync` and gives no signal that this matters. If your embedding
pattern tears down runtimes between calls, either keep one long-lived
runtime for any kernel that backgrounds work, or avoid `&` entirely on a
per-call runtime.

### A hard-killed kaish process can orphan its external children

`setpgid` + a pidfd kill, and tokio's `kill_on_drop`, all need *your* process
to still be alive and running code. None of them fire on `kill -9`, a
segfault, or an OOM kill — so an external command started under an embedder
that dies that way keeps running, unreparented to anything that will stop it.

`KernelConfig::with_kill_children_on_parent_death(true)` arms Linux's
`PR_SET_PDEATHSIG(SIGKILL)` in each child's `pre_exec`, so the OS kills it the
instant the parent dies, for any reason, with no cleanup path of ours
involved:

```rust
let config = KernelConfig::default().with_kill_children_on_parent_death(true);
```

On by default for `KernelConfig::agent()` and `agent_with_root()`, off
everywhere else — the same "protection on for the agent preset, opt in
elsewhere" split `vfs_budget_bytes` uses. It is not unconditional because it
costs something a human at a REPL may not want: an armed child cannot outlive
its shell at all, and cannot opt out from inside (unlike SIGHUP, which
`nohup`/`disown` exist to escape). A REPL user who backgrounds a long download
and exits expects it to survive; an agent embedder expects the opposite.

**Linux only.** macOS has no `PR_SET_PDEATHSIG`, and no equivalent that works
without a live watcher process (`kqueue`'s `NOTE_EXIT` needs one). The flag is
accepted and has no effect there rather than being faked with something
weaker — a macOS embedder that needs the guarantee supplies it from outside
the process (a process group the supervisor kills, or a container).

## Argv-Native Execution: `execute_argv`

`Kernel::execute(&str)` is string-native — it lexes and parses its input. If your
embedder already holds **tokenized** arguments (a structured tool call, a
multicall-style frontend), re-quoting them into a string just to have the lexer
split them apart again is wasteful and **lossy**: `ToolArgs::to_argv()` — the
argv-reconstruction step builtins use internally to feed their clap parsers —
stringifies typed values, so a `Value::Json` record loses its structure in the
round-trip. A `Value::Bytes` blob is worse than lossy: in a **named/flag**
argument it's a loud error (`to_argv()` returns
`Result<Vec<String>, ToolArgvError>`, not a bare `Vec<String>`) rather than
silent corruption; in a **positional** argument it renders as an opaque
`[binary: N bytes]` placeholder without erroring, since a clap-reflected
positional field is a validation-only sink no builtin reads for its value.
`ToolArgs::to_argv_excluding(keys)` is the same reconstruction with given
named keys skipped entirely — for a tool that deliberately reads one of its
own params raw off `args.named` (to preserve a `Value::Bytes` payload past the
argv/text boundary) instead of through the round-trip (`write`'s `content`
param does this). `execute_argv` is the peer door that skips the round-trip
entirely:

```rust
use kaish_kernel::ast::Value;

// Run one command whose arguments are already tokenized.
let result = kernel.execute_argv("grep", &[
    Value::String("--ftype".into()),
    Value::String("rust".into()),
    Value::String("needle".into()),
    Value::String("src".into()),
]).await?;

// Typed values pass straight into ToolArgs.positional — no stringification.
let result = kernel.execute_argv("my-tool", &[Value::Bytes(blob)]).await?;
```

Semantics:

- **Tokens are literal.** No glob expansion, no `$VAR` interpolation, no command
  substitution, no word splitting — the "single-quoted word" rule taken to its
  end. `execute_argv("echo", &[Value::String("*.txt".into())])` emits `*.txt`.
  And no **number coercion**: a `Value::String("00")` stays `"00"` (the string
  door's lexer would coerce the bare word `00` to an integer and print `0`). Pass
  a `Value::Int`/`Value::Float` when you mean a number — the type is yours to
  choose, which is the point of the typed door. **Exception:** a leading `~` is
  expanded against the session `HOME`, matching the string door (kaish expands
  `~` uniformly, even in quotes — so the doors agree); pass a pre-resolved path
  if you need it byte-literal.
- **One simple command only.** Pipelines, `&&`/`||`, control flow, and `$()` have
  no argv encoding — use `execute(&str)` for those. The two are *peers*: argv is
  not a subset that drops expressiveness, it's a different door that converges with
  the string door at the shared dispatch chain.
- **Same tail as the string door.** Command resolution (aliases, user tools,
  `.kai` scripts, externals, backend tools), `--json`, and the approval gate
  all apply, so a gated `rm` still returns exit 2 with a request on
  `ExecResult.approval` (see [Destructive-op rails](#destructive-op-rails-inspecting-and-fulfilling-an-approval-gate)).
  The kernel's pre-execution *syntax* validator does not run — argv carries no
  shell syntax — but a tool's own `validate()`/clap parse still does.
- **Typed-passthrough caveat.** Because builtins re-parse their own `to_argv()`
  internally (the two-layer clap model), the un-stringified-value win fully lands
  only for tools that read `args.positional` directly (the documented pattern),
  not those that trust their clap struct after a `to_argv()` round-trip. A
  `Value::Bytes` passed as a **named** argument to such a tool surfaces as the
  tool's own `to_argv()` failure (`ExecResult::failure`), not a silent stringify.

Concurrent callers serialize on the same execute lock as `execute`, and the
kernel's configured `request_timeout` applies (a hung builtin or external is
interrupted at the deadline with exit code 124). There is no per-call options
surface yet — if you need per-call timeout/cancel/vars/cwd, use the string door
(`execute_with_options`) until an `execute_argv_with_options` lands.

## Custom Tools

Register custom builtins using the `configure_tools` callback on
`with_backend()` (or a `ToolRegistry` you pass to your backend). The `Tool`
trait lives in `kaish-tool-api` and is re-exported by the kernel:

```rust
use std::sync::Arc;
use async_trait::async_trait;
use kaish_kernel::{Kernel, Tool};
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_types::ExecResult;

struct MyTool {
    state: Arc<MyState>,
}

#[async_trait]
impl Tool for MyTool {
    fn name(&self) -> &str { "my-tool" }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("my-tool", "Does something useful")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        ExecResult::success("hello from my-tool")
    }
}

let kernel = Kernel::with_backend(backend, config, |_| {}, |tools| {
    tools.register(MyTool { state: my_state.clone() });
})?;
```

Custom tools registered this way are available as shell builtins — they
appear in `tools --json`, have help text, and participate in tab
completion.

Notes:

- `ctx` is `&mut dyn ToolCtx` — a capability trait giving VFS access, cwd,
  stdin, and cancellation without depending on kernel internals.
- If your tool renders its own output (including handling `--json`
  itself), mark the schema `.with_owned_output()` — the kernel then passes
  `--json` through instead of re-rendering your `ExecResult`. It also passes
  `--help`/`-h` through: an owned-output tool re-parses its own argv, so the
  kernel's generic whole-tool help router stands aside and lets the tool render
  its own help (including leaf/subcommand help its internal parser knows about).
  This makes `--help`/`-h` handling **your** responsibility — unlike `--json`,
  there is no post-execute safety net. If you re-parse with clap this is
  automatic (clap emits help on `--help`); a hand-rolled parser must handle it
  explicitly, or `--help` will fall into your default action.

### Patient tools: suspending the script timeout

The script timeout (`ExecuteOptions::timeout` / `KernelConfig::request_timeout`)
is one budget for the whole script — sized for shell work, not for a
model-backed tool whose provider call legitimately runs minutes. Stretching
the script budget to minutes would hand a `while true` loop the same minutes,
so the two jobs get separate knobs: a tool declares its own budget with
`ctx.patient`.

```rust
async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
    let cancel = /* clone ctx.cancel via the ExecContext downcast */;

    // While the guard is held, the script clock is frozen and this hold's
    // own budget governs; dropping it resumes the script clock with the
    // remaining time it had at acquire.
    let _guard = ctx.patient(Duration::from_secs(300));

    tokio::select! {
        result = call_provider(args) => to_exec_result(result),
        _ = cancel.cancelled() => ExecResult::failure(130, "interrupted"),
    }
}
```

Semantics:

- **The hold's budget has teeth**: if the tool outlives it, the watchdog
  fires and the script exits 124 — a hung provider call cannot wait forever.
- **Cancellation stays live**: `Kernel::cancel()` and the embedder
  `cancel_token` fire immediately during a hold — only the timer pauses.
  A patient tool must still `select!` its wait against `ctx.cancel`,
  as above.
- **Script code has no path to the guard** — only Rust tool code can be
  patient, so the script-level budget keeps its teeth against shell loops.
- **The `timeout` builtin is not suspended**: `timeout 5 my-tool` is an
  explicit user bound on the command and ignores patient holds.
- With no script timeout configured the guard is inert (nothing to
  suspend); holds nest, and the guard may be held across `.await` points.

## Sandboxing and External Commands

Builtins go through the VFS and respect its mounts; **external commands,
`exec`, and `spawn` access the real filesystem directly** (they're OS
processes). Two gates:

- Compile-time: build without the `subprocess` feature — the capability
  doesn't exist.
- Runtime: `allow_external_commands = false` in `KernelConfig` — PATH
  lookups return "command not found" and `exec`/`spawn` error.
  `KernelConfig::isolated()` sets this by default.

### Preflighting a script for external commands

To gate a script for consent (e.g. block until external commands are approved),
classify each command node *before* executing. Walk the parsed AST and ask the
kernel how it will resolve each name — don't re-derive the rules, or your gate
silently disagrees with what kaish actually runs the day resolution changes:

```rust
use kaish_kernel::{parser, ast::Stmt, CommandKind};

let program = parser::parse(src)              // public parser + AST
    .map_err(|_errors| /* surface parse errors */ ())?;
for stmt in &program.statements {
    if let Stmt::Command(cmd) = stmt {        // walk however your policy needs
        let kind = kernel.classify_command(&cmd.name).await;
        if kind.escapes_kernel() {
            // External or Dynamic — escapes to PATH (or can't be resolved
            // statically). Gate it.
        }
        // Builtin / UserTool / Special run in-process under the VFS and
        // capability model.
    }
}
```

`CommandKind` is `#[non_exhaustive]`, so a `match` needs a wildcard arm — and the
safe default for an unrecognized kind is to gate it. `escapes_kernel()` captures
the two buckets a consent gate scrutinizes without spelling out the variants.

`classify_command` mirrors the interpreter's real resolution order — including
**alias expansion** — so a name like `readonly` (no kaish builtin; resolves to an
external binary) reports `External`, and an `alias cat=/bin/something` makes `cat`
report `External` too, the same thing it would actually run. The safe direction of
any residual imprecision is `External`/`Dynamic` — it never under-reports a `PATH`
escape as internal (`/v/bin/cat` and `.kai`/backend tools over-report as
`External`). The consent UX and the block-the-script loop are embedder policy —
the kernel supplies only the classification.

### Statement metadata without the ledger: `plan_program`

`plan_program(source, redactor)` returns one `PlannedStatement` per statement of
`source` — its position (`index`) and its `Plan`: the statement rendered back
to shell text **unexpanded**, its kind, and every command it contains, loop
bodies and `$(...)` substitutions included. Nothing executes: a plan is parse
information, and `${HOME}` appears as written. `Kernel::plan_program(source)`
is the same read through the kernel's installed `Redactor`, so a secret
argument reads as redacted here exactly as it would in the kernel's own record.

This is the same walk that feeds the kernel's statement gate, and the two stay
correlated end-to-end:

- `PlannedStatement.index` is the index a held statement's
  `ResumeAction::ConfirmStatement` quotes and `Kernel::confirm` replays.
- The grant's binding digest is computable from the metadata alone: SHA-256 of
  `strip_confirm_tokens(plan.rendered)`.
- Every literal `--confirm=<key>` is redacted from the plans and **not
  returned** — the caller holds `source` and needs no second copy of its
  credentials.

```rust
use kaish_kernel::plan_program;

for planned in plan_program(src, None).map_err(|_errors| /* parse errors */ ())? {
    for cmd in &planned.plan.commands {
        // cmd.name, cmd.args (redaction-aware), cmd.redirects, cmd.background
    }
    // Judge planned.plan however your policy needs, keyed by planned.index.
}
```

What this surface does **not** give you is the enforcement the ledger keeps:
single-successful-run redemption, revision checks, and condition re-checks are
decisions made under the kernel's lock about state only it holds, and metadata
cannot replace them. Compose policy over the plans; leave redemption to the
kernel.

## Path Composition with XDG Primitives

kaish exports XDG base directory primitives so embedders can compose their
own application-specific paths:

```rust
use kaish_kernel::{
    xdg_data_home,    // ~/.local/share or $XDG_DATA_HOME
    xdg_config_home,  // ~/.config or $XDG_CONFIG_HOME
    xdg_cache_home,   // ~/.cache or $XDG_CACHE_HOME
    xdg_runtime_dir,  // $XDG_RUNTIME_DIR or /tmp
    home_dir,         // ~ or $HOME
};

fn myapp_data_dir() -> PathBuf {
    xdg_data_home().join("myapp")
}
```

For user-facing path handling, use `expand_tilde`:

```rust
use kaish_kernel::expand_tilde;

let path = expand_tilde("~/projects/myrepo");
// → /home/username/projects/myrepo
```

## Programmatic VFS Access

The `Filesystem` trait (from `kaish-vfs`, re-exported as
`kaish_kernel::vfs::Filesystem`) takes `&Path`, not `&str`:

```rust
use std::path::Path;
use kaish_kernel::vfs::Filesystem;

let data = kernel.vfs().read(Path::new("/v/jobs/1/status")).await?;
```

## Job Output Capture

kaish provides bounded streams for capturing command output without OOM
risk.

### BoundedStream for Custom Output Capture

```rust
use kaish_kernel::{BoundedStream, drain_to_stream, DEFAULT_STREAM_MAX_SIZE};
use std::sync::Arc;
use tokio::process::Command;

async fn capture_with_bounds() -> anyhow::Result<String> {
    let mut child = Command::new("some-chatty-command")
        .stdout(std::process::Stdio::piped())
        .spawn()?;

    // Create bounded stream (10MB max, oldest data evicted on overflow)
    let stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

    // Drain stdout into the bounded stream
    if let Some(stdout) = child.stdout.take() {
        let stream_clone = stream.clone();
        tokio::spawn(async move {
            drain_to_stream(stdout, stream_clone).await;
        });
    }

    child.wait().await?;

    // Read captured output (safe even if process wrote gigabytes)
    Ok(stream.read_string().await)
}
```

### Sharing one JobManager across kernels

Each kernel builds its own `JobManager` unless you supply one. An embedder
that builds a kernel per request loses every `cmd &` job when that kernel
drops — ids, status, and output all live on the manager. Hand the same
manager to every kernel and jobs survive between calls:

```rust
use std::sync::Arc;
use kaish_kernel::scheduler::{JobId, JobManager};
use kaish_kernel::{Kernel, KernelConfig};

// Built once, held for the process's lifetime.
let jobs = Arc::new(JobManager::new());

// Every per-request kernel adopts it.
let kernel = Kernel::new(KernelConfig::agent().with_job_manager(jobs.clone()))?;
kernel.execute("cargo build &").await?;
drop(kernel);

// The next kernel sees job 1 — same table, same id space.
let next = Kernel::new(KernelConfig::agent().with_job_manager(jobs.clone()))?;
assert!(next.jobs().exists(JobId(1)).await);
```

A shared manager carries shared settings: `kill_grace` and
`persist_output_files` are stamped onto it at kernel construction, so the
last kernel built wins for both. A hermetic kernel (`NoLocal`, or any
`with_backend` kernel) turns `persist_output_files` off for every kernel on
that manager. Share a manager between kernels configured alike, or accept
the last writer.

### JobFs for Background Job Observability

The kernel automatically mounts `JobFs` at `/v/jobs`, exposing background
job state:

```
/v/jobs/
├── 1/
│   ├── status    # "running", "stopped", "done:0", "gated", "killed:N", or "failed:N"
│   ├── command   # Original command string
│   ├── stdout    # Job's stdout so far — live while it runs
│   ├── stderr    # Job's stderr so far — live while it runs
│   └── approval  # Pending approval request (JSON) if gated, else empty
├── 2/
│   └── ...
```

```sh
# In kaish scripts
cargo build 2>&1 &          # Starts job 1, returns immediately
jobs                        # Shows: [1] running  /v/jobs/1/
cat /v/jobs/1/status        # "running"
cat /v/jobs/1/stdout        # Whatever the build has printed so far
```

`stdout` and `stderr` are live for an **external** command run by the job:
its drain task tees each 8 KiB chunk into the node as the child emits it.
GH #240 had removed both nodes because they filled only once, at completion,
while four docs promised a live stream — they are back on the terms the docs
always claimed.

Three limits, stated because an embedder polling these needs to predict them:

- **A builtin is not a live producer.** A kaish builtin returns its whole
  output as a value when it finishes, so `echo hi &` fills the node in one
  write at completion — and so does `cargo build 2>&1 | tee build.log &`,
  because kaish's `tee` is a builtin. Drop the `| tee`: the job's own stream
  *is* the log.
- **Only the last stage of a pipeline reaches `stdout`.** An upstream stage's
  output is the next stage's stdin, not the job's stdout. `stderr` takes every
  stage's, since stderr is not piped. One consequence: in a job mixing
  builtins and externals, once any external has written stderr the
  completion write is skipped, so a builtin stage's stderr stays in the job's
  `ExecResult` and does not reach the node.
- **Each node is a 10 MB ring** that evicts its oldest bytes. A job that
  outruns it loses its head, not its tail; redirect to a file
  (`cmd > /tmp/out.log &`) when the whole output matters.

From Rust, `JobManager::read_stdout(id)` / `read_stderr(id)` return the same
snapshot (`None` for an unknown job, `Some(vec![])` for one that has written
nothing yet). To tail a job without a poll loop, take
`JobManager::streams(id)` and await `BoundedStream::changed_since`:

```rust
let streams = kernel.jobs().streams(id).await.expect("job exists");
let mut seen = 0;
loop {
    let stats = streams.stdout.changed_since(seen).await;
    seen = stats.total_written;
    // ... consume streams.stdout.read().await ...
    if stats.closed {
        break; // the job finished; nothing more is coming
    }
}
```

The streams close when the job's result is in, so `stats.closed` is the
caller's stop condition — no timeout guessing.

A destructive op backgrounded under `set -o approvals` (`rm x &`) gates in the
background rather than running: `status` is `gated`, `JobInfo.approval` (from
`JobManager::list`/`get`) and `/v/jobs/{id}/approval` (JSON) carry the pending
request, and `wait` surfaces it on the result's `.approval` field (exit 2). The
request carries the `job_id` it was posted for, stamped once at post time, so
every one of those surfaces reports the same correlation. Waiting on several
gated jobs still surfaces **one** request on `.approval` — one operation, one
request — while the message names the total (`wait: 3 approvals pending — run
`approvals list``); `/v/approvals/pending` enumerates all of them. An embedder fulfills
the backgrounded gate with `Kernel::confirm(&handle, &id)` — the
same API as a foreground gate.

The status strings are exactly `running`, `stopped`, `done:0`, `gated`,
`killed:{code}`, and `failed:{code}` — match on those, not on `completed`.
`killed:{code}` marks a job terminated by `kill %N` (or an embedder cancel):
the job stays tracked with its result and output until reaped, so a killed
job is still distinguishable from one that never existed, and `wait %N` still
returns its result (GH #244). `kill %N` waits for the job to actually unwind
(bounded by `kill_grace` + 3s) before exiting 0; `kill --no-wait %N` returns
at dispatch. The `JobManager` keeps at most 100 finished jobs — enforced at
registration and whenever completion is observed (`list`, `wait`); oldest
evicted first, gated jobs never evicted — tune with
`JobManager::set_finished_retention`. A session that registers jobs and then
never calls anything holds what it registered; there is no background
sweeper.

`JobId`/`JobStatus`/`JobInfo` (`kaish-types`) implement `Serialize`/
`Deserialize` (plus `schemars::JsonSchema` behind the `schema` feature), so an
embedder can serialize `JobManager::list()`/`get()` output directly rather
than hand-rolling a mirror struct. `JobStatus`'s wire spelling under
serde is lowercase (`"running"`/`"stopped"`/`"done"`/`"gated"`/`"killed"`/`"failed"`),
matching the `/v/jobs/N/status` text vocabulary above — not the capitalized
`Display` impl used for human-facing text (the `jobs` table). `JobInfo` also
carries `exit_code: Option<i64>` (set once the job finishes), `started_at` /
`finished_at: Option<SystemTime>` (acquired via `kaish_types::clock`, so they
work on `wasm32-unknown-unknown` too), and `pgids: Vec<u32>` — the real OS
process groups a background job spawned. `pgids` is the surface to use for
"what is this job actually doing"; `pid` is set only for a Ctrl-Z-stopped
foreground job (a TTY concept an embedder never sees) and is otherwise
`None`. For a finished job's `ExecResult` without blocking, use the
non-blocking `JobManager::try_result(id) -> Option<ExecResult>` instead of
`wait`, which parks until the job completes.

`JobManager::list`/`list_ids` return jobs sorted ascending by `JobId` (GH
#247) — job ids are minted in strictly increasing order, so this is spawn
order. Before this, both iterated the backing `HashMap` directly, so two
jobs could come back as `[2, 1]`: arbitrary, and a flake source for any
caller (an MCP surface handing an agent a job list, a snapshot test) that
depended on the order.

### Shutting down a kernel

`Kernel::shutdown(&self)` (GH #245) cancels every tracked background job
([`Kernel::cancel_all_jobs`], the same lever `kill %N` uses — an in-process
builtin future exits at its next checkpoint, an external child gets
SIGTERM→SIGKILL), then waits up to `kill_grace + 3s` **per job** for it to
actually unwind — mirroring `kill %N`'s own bound. The waits are sequential,
so the worst case is additive: N jobs that all ignore cancellation block
`shutdown` for N × (kill_grace + 3s); jobs that unwind promptly cost only
their own unwind time. A job that has not
unwound by its deadline is logged (`tracing::warn!`) and abandoned: it keeps
running detached until the tokio runtime itself goes away. Before this fix
`shutdown` called `JobManager::wait_all()` with no timeout at all — a single
`sleep 3600 &` blocked it for an hour.

`shutdown` takes `&self`, not owned `self`, specifically so an embedder
holding `Arc<Kernel>` (as `EmbeddedClient` does) can call it without
`Arc::try_unwrap` — the work only touches the shared `Arc<JobManager>`,
never kernel state that would need exclusive ownership. `EmbeddedClient::
shutdown` now calls straight through to it; it used to be a no-op with a
comment claiming the kernel's `Drop` would clean up background jobs. That
was never true: background jobs are detached `tokio::spawn` tasks holding
their own `Arc<Kernel>` **fork** ([`Kernel::fork_for_background`] mints an
independent `Arc`), not a reference back to the parent kernel, so dropping
the parent kernel neither cancels a running job nor waits for it — nor does
`Kernel` implement `Drop` at all. Call `shutdown()` explicitly before
dropping a kernel that may have backgrounded work; there is no other way to
stop it short of `kill %N` on every job.

## Frontend Completion Helpers (`kaish_client::completion`)

Answering Tab in a frontend (a REPL, a browser playground, any custom UI
around the kernel) needs two things: figuring out *what* the cursor is
completing, and turning a live kernel's schemas/vars into candidate
spellings. Both are extracted into `kaish_client::completion` so every
frontend shares one implementation instead of re-deriving it — the bundled
`kaish-repl` and the kaish-extras browser playground both consume this crate
rather than duplicating the logic.

```rust
use kaish_client::completion::{
    detect_completion_context, word_start, current_command, flag_candidates,
    CompletionContext,
};

// What kind of thing is being completed at `pos` in `line`?
match detect_completion_context(line, pos) {
    CompletionContext::Command => { /* complete a tool/alias name */ }
    CompletionContext::Variable => { /* complete a $VAR / ${VAR */ }
    CompletionContext::Path => { /* complete a filesystem path, or a
                                     flag if the word starts with `-` */ }
}

let start = word_start(line, pos); // byte offset the word under the cursor begins at

// Given the governing command and its ToolSchema, offer canonical flag spellings
if let Some((cs, ce)) = current_command(line, pos) {
    let candidates = flag_candidates(&schema.params, &line[cs..ce]);
    // -> canonical "--long" and "-x" spellings; snake_case field-id aliases
    //    stay reachable as input but aren't offered as candidates
}
```

Context detection is pure (no kernel access needed); turning a
`CompletionContext` into actual candidates is the frontend's job — walk
`kernel.tool_schemas()` for commands/flags, `kernel.list_vars()` for
variables, `kernel.vfs()` for paths, as `kaish-repl` and kaish-extras both do.

## Exported Types

The `kaish_kernel` crate root re-exports the embedding surface:

- **Core**: `Kernel`, `KernelConfig`, `VfsMountMode`, `ExecuteOptions`,
  `CommandKind`, `KernelBackend`, `LocalBackend`, `Tool`, `ToolRegistry`,
  `ExecContext`, `OutputLimitConfig`
- **Jobs**: `BoundedStream`, `StreamStats`, `drain_to_stream`,
  `DEFAULT_STREAM_MAX_SIZE`, `JobFs`
- **Paths**: `home_dir`, `xdg_data_home`, `xdg_config_home`,
  `xdg_cache_home`, `xdg_runtime_dir`, `expand_tilde`
- **VFS** (module `kaish_kernel::vfs`): `Filesystem`, `VfsRouter`,
  `MemoryFs`, `LocalFs`, `MountInfo`
- **Approvals** (module `kaish_kernel::ledger`): `Ledger`, `Approvals`,
  `Requester`, `ApproverHandle`, `AttemptHandle`, `RequestChain`,
  `LedgerConfig`, `LedgerSink`, `LedgerSinkError`, `LedgerError`,
  `Clock`, `SystemClock`, `Policy`, `StateResolver`, `PathResolver`,
  `ResolverError`, `StatementClassifier`, `CommandNameClassifier`,
  `StatementPosture`, `StatementAssessment`, `ClassificationError`,
  `ExecutionContext`, `MountDescriptor`, `Redactor`, `RedactionMark`,
  `ApprovalOutcome`, `PendingApproval`, `ResumeAction`, `LedgerStream`,
  `WatchEvent`, `AssessmentRecorder`, `KernelOperation`

Pure data types (`ExecResult`, `OutputData`, `Value`, `ToolSchema`,
`ToolArgs`, …) live in the leaf crate `kaish-types`; the tool author API
(`Tool`, `ToolCtx`, `KernelBackend`) in `kaish-tool-api`. Depend on those
directly if you're writing tools without linking the whole kernel.

## Best Practices

1. **Use `with_backend()` for full control** — implement `KernelBackend`
   and let the hermeticity guarantees keep I/O inside your storage model.

2. **Use `ExecuteOptions` for per-call state** — vars, timeout, cwd,
   cancellation, trace context. Don't mutate kernel state between calls.

3. **Compose paths with XDG primitives** — don't hardcode paths.

4. **Start from the minimal feature set** — add `subprocess`/`host`
   only when the embedder needs them; the attack surface is named, not
   inherited.
